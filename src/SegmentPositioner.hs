module SegmentPositioner where

import Prelude hiding (Left, Right, filter, lines)
import Control.Monad.Except(MonadError, void, lift)
import Control.Monad.Primitive
import Data.Fixed
import Data.Foldable
import Data.Function(on)
import Data.Int
import Data.List(groupBy)
import Data.Proxy
import Data.Word
import Data.Vector as V
import Linear
import OpenCV as CV
import OpenCV.Extra.XFeatures2d
import OpenCV.Internal.C.Types
import OpenCV.ImgProc.FeatureDetection
import System.IO.Unsafe ( unsafePerformIO )

import Loop
import Track
import Transform
import TrackGeometry

import Debug.Trace

type FrameMat = Mat ('S ['D, 'D]) ('S 3) ('S Word8)

positionSegments :: FrameMat -> Track -> Track
positionSegments frame track = positionStraights (fmap (positionSegment frame) track)

positionSegment :: FrameMat -> Segment -> Segment
positionSegment frame segment = segment { position = positionTile frame segment, transform = transformTile frame segment }

positionTile :: FrameMat -> Segment -> Position
positionTile frame (Segment Straight p t) = p

positionTile frame segment =
  let meanCircleOrigin = mean (candidateCircles segment frame)
  in  moveFromCircleOrigin segment meanCircleOrigin

mean :: Fractional a => V.Vector a -> a
mean xs = V.sum xs / realToFrac (V.length xs)

pointsFromLineSegment :: LineSegment Int32 -> V2 (V2 Double)
pointsFromLineSegment (LineSegment p0 p1) = V2 ((realToFrac <$>) p0) ((realToFrac <$>) p1)

transformTile :: FrameMat -> Segment -> Transform
transformTile frame s@(Segment Straight p t) =
  let lines = candidateLines s frame
      angle = angleFromTransform t
      -- TODO: Is taking the mean of an angle bad (because modulus)?
      lineAngle = mean $ fmap (angleFromPoints . pointsFromLineSegment . fst ) lines
      -- lineAngle might be pointing in the opposite direction to the segment
      angle' = if abs(angle - lineAngle) < (pi/2) then lineAngle else (lineAngle + pi) `mod'` pi
    in if V.null lines then t else transformFromAngle t angle'

transformTile frame (Segment tile p t) = t

type EdgeMat = Mat ('S ['D, 'D]) ('S 1) ('S Word8)

candidateLines :: Segment -> FrameMat -> Vector (LineSegment Int32, StraightEdge)
candidateLines segment frame = V.mapMaybe (\line -> (,) line <$> isCandidateLine segment line ) (lines frame)

isCandidateLine :: Segment -> LineSegment Int32 -> Maybe StraightEdge
isCandidateLine (Segment Straight p t) line =
  let edges  = straightEdges (Segment Straight p t)
      maxDist = trackWidth t / 4.0
  in V.find (\edge -> (lineDistance line (straightEdgeStart edge) < maxDist) || (lineDistance line (straightEdgeStop edge) < maxDist)) edges

isCandidateLine Segment {}  _ = error "Expect Straight"

-- https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line#Line_defined_by_two_points
-- https://stackoverflow.com/a/2233538/21115
lineDistance :: LineSegment Int32 -> V2 Double -> Double
lineDistance (LineSegment start end) (V2 x0 y0) =
  let (V2 x1 y1) = realToFrac <$> start
      (V2 x2 y2) = realToFrac <$> end
      px = x2 - x1
      py = y2 - y1
      something =  px*px + py*py
      u =  ((x0 - x1) * px + (y0 - y1) * py) / something
      u' = if u > 1 then 1 else if u < 0 then 0 else u
      x = x1 + u' * px
      y = y1 + u' * py
      dx = x - x0
      dy = y - y0

      in sqrt(dx*dx + dy*dy)

toEdges :: FrameMat -> EdgeMat
toEdges frame = exceptError $ canny 30 200 Nothing CannyNormL1 frame

lines :: FrameMat -> Vector (LineSegment Int32)
lines frame = unsafePerformIO $ do
  imgM <- CV.thaw (toEdges frame)
  exceptErrorM $ houghLinesP 1 (pi / 180) 80 (Just 30) (Just 10) imgM

candidateCircles :: Segment -> FrameMat -> Vector CircleCenter
candidateCircles segment frame = filter (isCandidateCircle segment) (circles (transform segment) frame)

isCandidateCircle :: Segment -> CircleCenter -> Bool
isCandidateCircle (Segment Straight _ _) _ = error "Expect Left or Right"
isCandidateCircle (Segment tile p t) center =
  let origin = moveToCircleOrigin (Segment tile p t)
    in center `distance` origin < (trackWidth t / 2.0)

-- houghCircles returns a float, awkwardly
type CircleCenter = V2 Double
toCircleCenter :: Circle -> CircleCenter
toCircleCenter = (realToFrac <$>) . circleCenter

circles :: Transform -> FrameMat -> Vector CircleCenter
circles t frame =
  let minRadius = round $ innerCornerCircleRadius t * 0.9
      maxRadius = round $ innerCornerCircleRadius t * 1.1
      imgG = exceptError $ cvtColor bgr gray frame
      circles = houghCircles 2 1 Nothing (Just 20) (Just minRadius) (Just maxRadius) imgG
  in exceptError $ V.map toCircleCenter <$> circles

positionStraights :: Track -> Track
positionStraights = mkLoop . Data.Foldable.concat . positionStraights' . chunkStraights

positionStraights' :: [[Segment]] -> [[Segment]]
positionStraights' (xs:ys:zs:rest)
  | tile (Prelude.head ys) == Straight =
        let prev = exitPosition (Prelude.last xs)
            next = position (Prelude.head zs)
            dPrev = prev - position (Prelude.head ys)
            dNext = next - exitPosition (Prelude.last ys)
            dMean = mean (V.fromList [dPrev, dNext])
            ys' = Prelude.map (\y -> y { position = position y + dPrev }) ys
        in xs : positionStraights' (ys':zs:rest)
  | otherwise = xs : positionStraights' (ys:zs:rest)
positionStraights' xs = xs

chunkStraights :: Track -> [[Segment]]
chunkStraights = groupBy chunk . unfold
  where chunk s1 s2 = (Straight /= Track.tile s1) == (Straight /= Track.tile s2)

inpaintWallsMask :: FrameMat -> EdgeMat
inpaintWallsMask frame = exceptError $ do
  frameHSV <- cvtColor bgr hsv frame
  let lowLo = toScalar (V4  0  100 0  0   :: V4 Double)
  let lowHi = toScalar (V4  10 255 255 255 :: V4 Double)
  let highLo = toScalar (V4  170  100 0  0   :: V4 Double)
  let highHi = toScalar (V4  180 255 255 255 :: V4 Double)
  lowMask <- inRange frameHSV lowLo lowHi
  highMask <- inRange frameHSV highLo highHi
  lowMask `bitwiseOr` highMask

inpaintWalls :: FrameMat -> FrameMat
inpaintWalls frame = exceptError $
  inpaint 2 InpaintTelea frame (inpaintWallsMask frame)
