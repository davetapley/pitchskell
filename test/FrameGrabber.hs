{-# language DataKinds #-}
{-# language TemplateHaskell #-}

module FrameGrabber
    ( withFrames,
      TestMat
    ) where

import Control.Monad.Loops ( unfoldM )
import qualified OpenCV as CV
import Data.Word
import OpenCV.TypeLevel
import OpenCV.VideoIO.Types

type TestMat = CV.Mat ('S ['D, 'D]) 'D 'D

withFrames :: FilePath -> (TestMat -> a) -> IO [a]
withFrames fp f = do
    cap <- CV.newVideoCapture
    CV.exceptErrorIO $ CV.videoCaptureOpen cap (CV.VideoFileSource fp Nothing)
    frames <- unfoldM (CV.videoCaptureRetrieve cap)
    return $ map f frames
