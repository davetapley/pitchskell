module MaskDebug where

import Data.Foldable
import Data.Proxy
import Data.Word
import Linear
import OpenCV

import Colors
import Mask
import Track

type FrameMat = Mat ('S ['D, 'D]) ('S 3) ('S Word8)

drawTrackMask :: FrameMat -> Track -> FrameMat
drawTrackMask frame track =
  let [h, w] = miShape . matInfo $ frame
      segmentMask = Just . mask (w, h)
  in exceptError $ withMatM (h ::: w ::: Z) (Proxy :: Proxy (S 3)) (Proxy :: Proxy (S Word8)) grey $
    \imgM -> traverse_ (matCopyToM imgM zero frame . segmentMask ) track
