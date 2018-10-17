module TilePositionerDebug where

import Control.Monad.Except(MonadError, void)
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

transparent, white, black, blue, green, red :: Scalar
transparent = toScalar (V4 255 255 255   0 :: V4 Double)
white       = toScalar (V4 255 255 255 255 :: V4 Double)
black       = toScalar (V4   0   0   0 255 :: V4 Double)
blue        = toScalar (V4 255   0   0 255 :: V4 Double)
green       = toScalar (V4   0 255   0 255 :: V4 Double)
red         = toScalar (V4   0   0 255 255 :: V4 Double)

cornerCircleRadius :: Transform -> Double
cornerCircleRadius t =
  let p = V2 0 0
    in distance p (p + (t !* V2 1.32 0))

drawHough :: Transform -> FrameMat -> FrameMat
drawHough t frame = exceptError $ do
  edgeImg <- canny 30 200 Nothing CannyNormL1 frame
  edgeImgBgr <- cvtColor gray bgr edgeImg
  let [h, w] = miShape . matInfo $ frame
  withMatM (h ::: w ::: Z) (Proxy :: Proxy 3) (Proxy :: Proxy Word8) white $ \imgM -> do
      edgeImgM <- CV.thaw edgeImg
      lineSegments <- houghLinesP 1 (pi / 180) 80 (Just 30) (Just 10) edgeImgM
      void $ matCopyToM imgM (V2 0 0) edgeImgBgr Nothing
      for_ lineSegments $ \lineSegment -> do
        line imgM
             (lineSegmentStart lineSegment)
             (lineSegmentStop  lineSegment)
             red 2 LineType_8 0

      imgG <- cvtColor bgr gray frame
      let minRadius = round $ cornerCircleRadius t * 0.8
      let maxRadius = round $ cornerCircleRadius t * 1
      circles <- houghCircles 3 1 Nothing Nothing (Just minRadius) (Just maxRadius) imgG
      -- circles <- houghCircles 1.65 1 Nothing Nothing Nothing Nothing imgG
      for_ circles $ \c -> do
        circle imgM (round <$> circleCenter c :: V2 Int32) (round (circleRadius c)) blue 1 LineType_AA 0
        circle imgM (round <$> circleCenter c :: V2 Int32) (round (circleRadius c / 10 )) green 1 LineType_AA 0

inpaintWalls :: FrameMat -> (FrameMat, FrameMat)
inpaintWalls frame = exceptError $ do
  frameHSV <- cvtColor bgr hsv frame
  let lowLo = toScalar (V4  0  100 0  0   :: V4 Double)
  let lowHi = toScalar (V4  10 255 255 255 :: V4 Double)
  let highLo = toScalar (V4  170  100 0  0   :: V4 Double)
  let highHi = toScalar (V4  180 255 255 255 :: V4 Double)
  lowMask <- inRange frameHSV lowLo lowHi
  highMask <- inRange frameHSV highLo highHi
  fullMask <- lowMask `bitwiseOr` highMask

  maskBGR <- cvtColor gray bgr fullMask
  inpainted <- inpaint 2 InpaintTelea frame fullMask
  pure (maskBGR, inpainted)