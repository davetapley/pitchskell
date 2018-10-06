module TileMatcherDebug where

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
    in exceptError $ withMatM (h ::: w ::: Z) (Proxy :: Proxy (S 3)) (Proxy :: Proxy (S Word8))
               grey $ \imgM -> do
                 matCopyToM imgM (V2 0 0) frame (Just (mask (w, h) (head $ Loop.unfold track)))
                 matCopyToM imgM (V2 0 0) frame (Just (mask (w, h) ( Loop.unfold track !! 1)))
                 matCopyToM imgM (V2 0 0) frame (Just (mask (w, h) ( Loop.unfold track !! 2)))
                 matCopyToM imgM (V2 0 0) frame (Just (mask (w, h) ( Loop.unfold track !! 3)))
                 matCopyToM imgM (V2 0 0) frame (Just (mask (w, h) ( Loop.unfold track !! 4)))
                 matCopyToM imgM (V2 0 0) frame (Just (mask (w, h) ( Loop.unfold track !! 5)))
                 matCopyToM imgM (V2 0 0) frame (Just (mask (w, h) ( Loop.unfold track !! 6)))
                 matCopyToM imgM (V2 0 0) frame (Just (mask (w, h) ( Loop.unfold track !! 7)))
                 matCopyToM imgM (V2 0 0) frame (Just (mask (w, h) ( Loop.unfold track !! 8)))
                 matCopyToM imgM (V2 0 0) frame (Just (mask (w, h) ( Loop.unfold track !! 9)))
                 matCopyToM imgM (V2 0 0) frame (Just (mask (w, h) ( Loop.unfold track !! 10)))
                 matCopyToM imgM (V2 0 0) frame (Just (mask (w, h) ( Loop.unfold track !! 11)))
                 matCopyToM imgM (V2 0 0) frame (Just (mask (w, h) ( Loop.unfold track !! 12)))
                 matCopyToM imgM (V2 0 0) frame (Just (mask (w, h) ( Loop.unfold track !! 13)))
                 matCopyToM imgM (V2 0 0) frame (Just (mask (w, h) ( Loop.unfold track !! 14)))
                 matCopyToM imgM (V2 0 0) frame (Just (mask (w, h) ( Loop.unfold track !! 15)))
                 -- (mapM_ addTile (Loop.unfold track))

  -- where addTile segment = matCopyToM imgM (V2 0 0) frame (Just (mask (w, h) segment))

drawMask :: FrameMat -> Segment -> FrameMat
drawMask frame (Segment _ p t) = do
  let [h, w] = miShape . matInfo $ frame
    in exceptError $ withMatM (h ::: (w*4) ::: Z) (Proxy :: Proxy (S 3)) (Proxy :: Proxy (S Word8))
               grey $ \imgM -> do
                 matCopyToM imgM (V2 (w*0) 0) frame Nothing
                 matCopyToM imgM (V2 (w*1) 0) frame (Just (mask (w, h) (Segment Straight p t)))
                 matCopyToM imgM (V2 (w*2) 0) frame (Just (mask (w, h) (Segment Left p t)))
                 matCopyToM imgM (V2 (w*3) 0) frame (Just (mask (w, h) (Segment Right p t)))

