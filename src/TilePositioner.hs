module TilePositioner where

import Prelude hiding (Left, Right, filter)
import Control.Monad.Except(MonadError, void, lift)
import Control.Monad.Primitive
import Data.Foldable
import Data.Int
import Data.Proxy
import Data.Word
import Data.Vector
import Linear
import OpenCV as CV
import OpenCV.Extra.XFeatures2d
import OpenCV.Internal.C.Types
import OpenCV.ImgProc.FeatureDetection

import Track

type FrameMat = Mat ('S ['D, 'D]) ('S 3) ('S Word8)

postitionTiles :: Track -> FrameMat -> Track
postitionTiles = undefined

isCandidateCircle :: Position -> Transform -> Circle -> Bool
isCandidateCircle p t c =
  let trackWidth = distance p (p + (t !* V2 1.32 0))
    in distance (circleCenter c) (realToFrac <$> p) < (realToFrac trackWidth)

postitionTile :: Segment -> FrameMat -> Segment
postitionTile (Segment Straight p t) frame = Segment Straight p t

postitionTile (Segment Left p t) frame =
  let origin = round <$> (p + (t !* V2 0 (-0.5)))
      candidateCircles = filter (isCandidateCircle p t) (circles t frame)
    in Segment Left p t

postitionTile (Segment Right p t) frame = Segment Right p t

type EdgeMat = Mat ('S ['D, 'D]) ('S 1) ('S Word8)

edges :: FrameMat -> EdgeMat
edges frame = exceptError $ canny 30 200 Nothing CannyNormL1 frame

lines :: (PrimMonad m) => FrameMat -> m (Vector (LineSegment Int32))
lines frame = do
  imgM <- CV.thaw (edges frame)
  exceptErrorM $ houghLinesP 1 (pi / 180) 80 (Just 30) (Just 10) imgM

cornerCircleRadius :: Transform -> Double
cornerCircleRadius t =
  let p = V2 0 0
    in distance p (p + (t !* V2 1.32 0))

circles :: Transform -> FrameMat -> Vector Circle
circles t frame = do
  let minRadius = round $ cornerCircleRadius t * 0.8
  let maxRadius = round $ cornerCircleRadius t * 1.01
  let imgG = exceptError $ cvtColor bgr gray frame
  exceptError $ houghCircles 3.5 1 Nothing Nothing (Just minRadius) (Just maxRadius) imgG

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
