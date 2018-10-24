{-# language DataKinds #-}
{-# language FlexibleInstances #-}

module Lib
    ( someFunc
    ) where

import Control.Monad ( unless )
import qualified OpenCV as CV
import OpenCV.TypeLevel
import OpenCV.VideoIO.Types

someFunc :: IO ()
someFunc = do
    cap <- CV.newVideoCapture
    -- Open the first available video capture device. Usually the
    -- webcam if run on a laptop.
    CV.exceptErrorIO $ CV.videoCaptureOpen cap $ CV.VideoDeviceSource 1 Nothing
    isOpened <- CV.videoCaptureIsOpened cap

    if isOpened then
      (do
         w <- CV.videoCaptureGetI cap VideoCapPropFrameWidth
         h <- CV.videoCaptureGetI cap VideoCapPropFrameHeight

         putStrLn $ "Video size: " ++ show w ++ " x " ++ show h

         CV.withWindow "video" $ \window -> loop cap window)
      else putStrLn "Couldn't open video capture device"

  where
    loop cap window = do
      _ok <- CV.videoCaptureGrab cap
      mbImg <- CV.videoCaptureRetrieve cap
      case mbImg of
        Just img -> do
          -- Assert that the retrieved frame is 2-dimensional.
          let img' :: CV.Mat ('S ['D, 'D]) 'D 'D
              img' = CV.exceptError $ CV.coerceMat img
          CV.imshow window img'
          key <- CV.waitKey 20
          -- Loop unless the escape key is pressed.
          unless (key == 27) $ loop cap window
        -- Out of frames, stop looping.
        Nothing -> pure ()
