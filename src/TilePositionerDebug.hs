module TilePositionerDebug where

import Prelude hiding (lines)
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

import TilePositioner
import Track

transparent, white, black, blue, green, red :: Scalar
transparent = toScalar (V4 255 255 255   0 :: V4 Double)
white       = toScalar (V4 255 255 255 255 :: V4 Double)
black       = toScalar (V4   0   0   0 255 :: V4 Double)
blue        = toScalar (V4 255   0   0 255 :: V4 Double)
green       = toScalar (V4   0 255   0 255 :: V4 Double)
red         = toScalar (V4   0   0 255 255 :: V4 Double)

showHough :: Transform -> FrameMat -> FrameMat
showHough t frame = exceptError $ do
  edgesBgr <- cvtColor gray bgr (edges frame)
  let [h, w] = miShape . matInfo $ frame
  withMatM (h ::: w ::: Z) (Proxy :: Proxy 3) (Proxy :: Proxy Word8) white $ \imgM -> do
      void $ matCopyToM imgM (V2 0 0) edgesBgr Nothing
      lines' <- lines frame
      for_  lines' $ \lineSegment -> do
        line imgM
             (lineSegmentStart lineSegment)
             (lineSegmentStop  lineSegment)
             red 2 LineType_8 0

      imgG <- cvtColor bgr gray frame
      let minRadius = round $ cornerCircleRadius t * 0.8
      let maxRadius = round $ cornerCircleRadius t * 1
      circles <- houghCircles 3.5 1 Nothing Nothing (Just minRadius) (Just maxRadius) imgG
      -- circles <- houghCircles 1.65 1 Nothing Nothing Nothing Nothing imgG
      for_ circles $ \c -> do
        circle imgM (round <$> circleCenter c :: V2 Int32) (round (circleRadius c)) blue 1 LineType_AA 0
        circle imgM (round <$> circleCenter c :: V2 Int32) (round (circleRadius c / 10 )) green 1 LineType_AA 0

showInpaintWalls :: FrameMat -> FrameMat
showInpaintWalls frame = exceptError $ do
  let mask = inpaintWallsMask frame
  maskBGR <- cvtColor gray bgr mask
  let inpainted = inpaintWalls frame

  let [h, w] = miShape . matInfo $ frame
  withMatM (h ::: (w*3) ::: Z) (Proxy :: Proxy (S 3)) (Proxy :: Proxy (S Word8)) transparent $ \imgM -> do
    matCopyToM imgM (V2 (w*0) 0) frame Nothing
    matCopyToM imgM (V2 (w*1) 0) maskBGR Nothing
    matCopyToM imgM (V2 (w*2) 0) inpainted Nothing
