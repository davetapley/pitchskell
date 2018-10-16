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

type FrameMat = Mat ('S ['D, 'D]) ('S 3) ('S Word8)

transparent, white, black, blue, green, red :: Scalar
transparent = toScalar (V4 255 255 255   0 :: V4 Double)
white       = toScalar (V4 255 255 255 255 :: V4 Double)
black       = toScalar (V4   0   0   0 255 :: V4 Double)
blue        = toScalar (V4 255   0   0 255 :: V4 Double)
green       = toScalar (V4   0 255   0 255 :: V4 Double)
red         = toScalar (V4   0   0 255 255 :: V4 Double)

drawHough :: FrameMat -> FrameMat
drawHough frame = exceptError $ do
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
      circles <- houghCircles 1.5 1 Nothing Nothing Nothing Nothing imgG
      for_ circles $ \c -> do
        circle imgM (round <$> circleCenter c :: V2 Int32) (round (circleRadius c)) blue 1 LineType_AA 0
