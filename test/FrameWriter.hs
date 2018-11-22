module FrameWriter where

import Control.Monad ( replicateM )
import Control.Monad.Loops ( unfoldM )
import qualified OpenCV as CV
import Data.Word
import OpenCV
import OpenCV.TypeLevel
import OpenCV.VideoIO.Types
import OpenCV.Internal.C.Types
import OpenCV.Internal.VideoIO.Types
import OpenCV.VideoIO.VideoWriter
import OpenCV.Unsafe
import Data.Maybe

type TestMat = CV.Mat ('S ['D, 'D]) ('S 3) ('S Word8)

writeFrames :: FilePath -> [TestMat] -> IO ()
writeFrames fp frames = do
  let [h, w] = miShape (matInfo (head frames))
  wr <- videoWriterOpen $ VideoFileSink' (VideoFileSink fp "avc1" 30 (w, h))
  mapM_ (exceptErrorIO . videoWriterWrite wr . unsafeCoerceMat) frames
  exceptErrorIO $ videoWriterRelease wr

