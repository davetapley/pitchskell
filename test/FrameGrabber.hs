{-# language DataKinds #-}
{-# language TemplateHaskell #-}

module FrameGrabber where

import Control.Monad ( replicateM )
import Control.Monad.Loops ( unfoldM )
import qualified OpenCV as CV
import Data.Word
import OpenCV.TypeLevel
import OpenCV.VideoIO.Types
import OpenCV.Internal.VideoIO.Types
import Data.Maybe

type TestMat = CV.Mat ('S ['D, 'D]) 'D 'D

withFile :: FilePath -> IO CV.VideoCapture
withFile fp = do
    cap <- CV.newVideoCapture
    CV.exceptErrorIO $ CV.videoCaptureOpen cap (CV.VideoFileSource fp Nothing)
    return cap

getFrames :: FilePath -> IO [TestMat]
getFrames fp = do
    cap <- withFile fp

    -- videoCaptureRetrieve is supposed to return Nothing at the end of the file
    -- But it's broken on MacOS: https://stackoverflow.com/questions/13798795/opencv-capture-loops-video-does-not-detect-last-frame
    -- So just use the frame count instead.

    frameCount <- CV.videoCaptureGetI cap VideoCapPropFrameCount
    catMaybes <$> replicateM (fromIntegral frameCount) (grabRetrieve cap)

    where grabRetrieve cap = CV.videoCaptureGrab cap >> CV.videoCaptureRetrieve cap

withFrames :: FilePath -> (TestMat -> a) -> IO [a]
withFrames fp f = do
    frames <- getFrames fp
    return $ map f frames
