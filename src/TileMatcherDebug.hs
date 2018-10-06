module TileMatcherDebug where

import Control.Monad
import Data.Foldable
import Data.Proxy
import Data.Word
import OpenCV

import Prelude hiding (Left, Right)
import TileMatcher
import Track
import Linear
import qualified Loop

grey = toScalar (V4   128 128 128 255 :: V4 Double)

drawTrackMask :: FrameMat -> Track -> FrameMat
drawTrackMask frame track = do
  let [h, w] = miShape . matInfo $ frame
      segmentMask = Just . mask (w, h)

    in exceptError $ withMatM (h ::: w ::: Z) (Proxy :: Proxy (S 3)) (Proxy :: Proxy (S Word8))
       grey $ \imgM -> traverse_ (matCopyToM imgM (V2 0 0) frame . segmentMask ) track

drawTileMasks :: FrameMat -> Segment -> FrameMat
drawTileMasks frame (Segment _ p t) = do
  let [h, w] = miShape . matInfo $ frame
    in exceptError $ withMatM (h ::: (w*4) ::: Z) (Proxy :: Proxy (S 3)) (Proxy :: Proxy (S Word8))
               grey $ \imgM -> do
                 matCopyToM imgM (V2 (w*0) 0) frame Nothing
                 matCopyToM imgM (V2 (w*1) 0) frame (Just (mask (w, h) (Segment Straight p t)))
                 matCopyToM imgM (V2 (w*2) 0) frame (Just (mask (w, h) (Segment Left p t)))
                 matCopyToM imgM (V2 (w*3) 0) frame (Just (mask (w, h) (Segment Right p t)))

