{-# language DataKinds #-}
{-# language TemplateHaskell #-}

module FrameGrabber where

import Control.Monad.Loops ( unfoldM )
import qualified OpenCV as CV
import Data.Word
import OpenCV.TypeLevel
import OpenCV.VideoIO.Types

type TestMat = CV.Mat ('S ['D, 'D]) 'D 'D


withFile :: FilePath -> IO CV.VideoCapture
withFile fp = do
    cap <- CV.newVideoCapture
    CV.exceptErrorIO $ CV.videoCaptureOpen cap (CV.VideoFileSource fp Nothing)
    return cap

getFrames :: FilePath -> IO [TestMat]
getFrames fp = do
    cap <- withFile fp
    unfoldM (CV.videoCaptureGrab cap >> CV.videoCaptureRetrieve cap)

withFrames :: FilePath -> (TestMat -> a) -> IO [a]
withFrames fp f = do
    frames <- getFrames fp
    return $ map f frames
