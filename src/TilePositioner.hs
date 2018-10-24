module TilePositioner where

import Prelude hiding (Left, Right, filter)
import Control.Monad.Except(MonadError, void, lift)
import Control.Monad.Primitive
import Data.Foldable
import Data.Int
import Data.Proxy
import Data.Word
import Data.Vector as V
import Linear
import OpenCV as CV
import OpenCV.Extra.XFeatures2d
import OpenCV.Internal.C.Types
import OpenCV.ImgProc.FeatureDetection

import Track
import TrackGeometry

type FrameMat = Mat ('S ['D, 'D]) ('S 3) ('S Word8)

positionTiles :: Track -> FrameMat -> [Position]
positionTiles = undefined

positionTile :: Segment -> FrameMat -> Position
positionTile (Segment Straight p t) frame = p

positionTile segment frame =
  let meanCircleOrigin = mean (candidateCircles segment frame)
  in  moveFromCircleOrigin segment meanCircleOrigin

mean :: Fractional a => V.Vector a -> a
mean xs = V.sum xs / realToFrac (V.length xs)

type EdgeMat = Mat ('S ['D, 'D]) ('S 1) ('S Word8)

edges :: FrameMat -> EdgeMat
edges frame = exceptError $ canny 30 200 Nothing CannyNormL1 frame

lines :: (PrimMonad m) => FrameMat -> m (Vector (LineSegment Int32))
lines frame = do
  imgM <- CV.thaw (edges frame)
  exceptErrorM $ houghLinesP 1 (pi / 180) 80 (Just 30) (Just 10) imgM

candidateCircles :: Segment -> FrameMat -> Vector CircleCenter
candidateCircles segment frame = filter (isCandidateCircle segment) (circles (transform segment) frame)

isCandidateCircle :: Segment -> CircleCenter -> Bool
isCandidateCircle (Segment tile p t) center =
  let origin = moveToCircleOrigin (Segment tile p t)
    in center `distance` origin < (trackWidth t / 4.0)

-- houghCircles returns a float, awkwardly
type CircleCenter = V2 Double
toCircleCenter :: Circle -> CircleCenter
toCircleCenter = (realToFrac <$>) . circleCenter

circles :: Transform -> FrameMat -> Vector CircleCenter
circles t frame = do
  let minRadius = round $ outerCornerCircleRadius t * 0.8
  let maxRadius = round $ outerCornerCircleRadius t * 1.2
  let imgG = exceptError $ cvtColor bgr gray (inpaintWalls frame)
  let circles = houghCircles 3.5 1 Nothing Nothing (Just minRadius) (Just maxRadius) imgG
  exceptError $ V.map toCircleCenter <$> circles

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
