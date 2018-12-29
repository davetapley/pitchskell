module Image where

import qualified Data.ByteString as B
import Data.Maybe
import Data.Word
import Linear
import OpenCV as CV
import OpenCV.Core.Types.Mat
import System.IO.Unsafe ( unsafePerformIO )

import qualified Track
import Transform

renderImage
    :: FilePath
    -> CV.Mat ('CV.S [h, w]) channels depth
    -> IO ()
renderImage fp img = do
    let bs = CV.exceptError $ CV.imencode (CV.OutputPng CV.defaultPngParams) img
    B.writeFile fp bs

type FrameMat = Mat ('S ['D, 'D]) ('S 3) ('S Word8)

idleNoCars :: FrameMat
idleNoCars =
    CV.exceptError $ coerceMat $ unsafePerformIO $
      CV.imdecode CV.ImreadUnchanged <$> B.readFile "test/images/idle-no-cars-0.png"

idleNoCarsStart = Track.Segment Track.Straight (V2 485 141) $ mkTransform (V2 (V2 (-47) (-1)) (V2 1 (-47)))
idleNoCarsTrack = fromJust $ Track.parseTrack idleNoCarsStart "sslrlsllrsslrlls"
