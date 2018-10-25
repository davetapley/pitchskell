module TilePositionerDebug where

import Prelude hiding (Left, lines)
import Control.Monad.Except(MonadError, void)
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

import TilePositioner
import Track
import TrackGeometry
import Colors

positionCircleDebug :: FrameMat -> Segment -> FrameMat
positionCircleDebug frame (Segment tile p t) = exceptError $ do
  let p' = positionTile frame (Segment tile p t)
  let [h, w] = miShape . matInfo $ frame
  withMatM (h ::: w ::: Z) (Proxy :: Proxy 3) (Proxy :: Proxy Word8) white $ \imgM -> do
    void $ matCopyToM imgM (V2 0 0) frame Nothing
    let dot = round $ trackWidth t / 32.0
    for_ (candidateCircles (Segment tile p t) frame) $ \c -> circle imgM (round <$> c :: V2 Int32) dot blue 1 LineType_AA 0

    circle imgM  (round <$> p) dot white (-1) LineType_AA 0
    circle imgM  (round <$> p') dot green (-1) LineType_AA 0

showHough :: Transform -> FrameMat -> FrameMat
showHough t frame = exceptError $ do
  edgesBgr <- cvtColor gray bgr (edges frame)
  let [h, w] = miShape . matInfo $ frame
  withMatM (h ::: w ::: Z) (Proxy :: Proxy 3) (Proxy :: Proxy Word8) white $ \imgM -> do
      void $ matCopyToM imgM (V2 0 0) edgesBgr Nothing
      lines' <- lines frame
      for_  lines' $ \lineSegment -> line imgM (lineSegmentStart lineSegment) (lineSegmentStop  lineSegment) red 2 LineType_8 0

      imgG <- cvtColor bgr gray frame
      let minRadius = round $ outerCornerCircleRadius t * 0.9
      let maxRadius = round $ outerCornerCircleRadius t * 1.1
      let dot = round $ trackWidth t / 32.0
      circles' <- houghCircles 4 1 Nothing Nothing (Just minRadius) (Just maxRadius) imgG
      for_ circles' $ \c -> do
        circle imgM (round <$> circleCenter c :: V2 Int32) (round (circleRadius c)) blue 1 LineType_AA 0
        circle imgM (round <$> circleCenter c :: V2 Int32) dot green (-1) LineType_AA 0

showInpaintWalls :: FrameMat -> FrameMat
showInpaintWalls frame = exceptError $ do
  let mask = inpaintWallsMask frame
  maskBGR <- cvtColor gray bgr mask
  let inpainted = inpaintWalls frame

  let [h, w] = miShape . matInfo $ frame
  withMatM (h ::: (w*3) ::: Z) (Proxy :: Proxy (S 3)) (Proxy :: Proxy (S Word8)) transparent $ \imgM -> do
    matCopyToM imgM (V2   0   0) frame Nothing
    matCopyToM imgM (V2   w   0) maskBGR Nothing
    matCopyToM imgM (V2 (w*2) 0) inpainted Nothing
