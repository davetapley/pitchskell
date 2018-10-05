module TileMatcherDebug where

import Data.Proxy
import Data.Word
import OpenCV

import Prelude hiding (Left, Right)
import TileMatcher
import Track
import Linear

grey = toScalar (V4   128 128 128 255 :: V4 Double)

drawMask :: FrameMat -> Segment -> FrameMat
drawMask frame (Segment _ p t) = do
  let [h, w] = miShape . matInfo $ frame
    in exceptError $ withMatM (h ::: (w*4) ::: Z) (Proxy :: Proxy (S 3)) (Proxy :: Proxy (S Word8))
               grey $ \imgM -> do
                 matCopyToM imgM (V2 (w*0) 0) frame Nothing
                 matCopyToM imgM (V2 (w*1) 0) frame (Just (mask (w, h) (Segment Straight p t)))
                 matCopyToM imgM (V2 (w*2) 0) frame (Just (mask (w, h) (Segment Left p t)))
                 matCopyToM imgM (V2 (w*3) 0) frame (Just (mask (w, h) (Segment Right p t)))
