{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module StartFiducial where

import Data.Int
import Data.Proxy
import Linear
import OpenCV
import OpenCV.Extra.XFeatures2d

-- Inspired by  https://github.com/LumiGuide/haskell-opencv/blob/5fe41ffe54bf850a65aeef8a507753ade61a44cf/opencv-extra/src/OpenCV/Extra/XFeatures2d.hs#L302
startDetectAndComputeImg frame = exceptError $ do
    (kpts, _descs) <- siftDetectAndCompute sift frame Nothing
    withMatM (Proxy :: Proxy [height, width])
             (Proxy :: Proxy channels)
             (Proxy :: Proxy depth)
             white $ \imgM -> do
      void $ matCopyToM imgM (V2 0 0) frame Nothing
      for_ kpts $ \kpt -> do
        let kptRec = keyPointAsRec kpt
        circle imgM (round <$> kptPoint kptRec :: V2 Int32) 5 red 1 LineType_AA 0
  where
    sift = mkSift defaultSiftParams
