module TileMatcherDebug where

import Control.Monad
import Data.Foldable
import Data.Proxy
import Data.Word
import OpenCV

import qualified Data.Text as T

import Prelude hiding (Left, Right)
import TileMatcher
import Track
import Linear
import Linear.V2
import Linear.V3
import Linear.V4

import qualified Loop

grey = toScalar (V4   128 128 128 255 :: V4 Double)

drawTrackMask :: FrameMat -> Track -> FrameMat
drawTrackMask frame track =
  let [h, w] = miShape . matInfo $ frame
      segmentMask = Just . mask (w, h)
  in exceptError $ withMatM (h ::: w ::: Z) (Proxy :: Proxy (S 3)) (Proxy :: Proxy (S Word8)) grey $
    \imgM -> traverse_ (matCopyToM imgM zero frame . segmentMask ) track

drawTileMasks :: FrameMat -> Segment -> FrameMat
drawTileMasks frame (Segment _ p t) =
  let [h, w] = miShape . matInfo $ frame
  in exceptError $ withMatM (h ::: (w*4) ::: Z) (Proxy :: Proxy (S 3)) (Proxy :: Proxy (S Word8)) grey $
    \imgM -> do
      matCopyToM imgM (V2 (w*0) 0) frame Nothing

      let copyMask n tile = matCopyToM imgM (V2 (w*n) 0) frame (Just (mask (w, h) (Segment tile p t)))
          putOverlap n tile = putText imgM (T.pack $ showTileOverlap frame p t tile) (V2 (50+(w*n)) 50) (Font FontHersheySimplex NotSlanted 0.6) white 1 LineType_AA False

      copyMask 1 Straight
      putOverlap 1 Straight
      copyMask 2 Left
      putOverlap 2 Left
      copyMask 3 Right
      putOverlap 3 Right

showTileOverlap :: FrameMat -> Position -> Transform -> Tile -> String
showTileOverlap frame p t tile = show (tileOverlap frame $ Segment tile p t)
