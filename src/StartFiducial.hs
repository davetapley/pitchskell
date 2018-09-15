{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module StartFiducial where

import Control.Monad
import Data.Foldable
import Data.Int
import Data.Proxy
import GHC.TypeLits
import Linear
import OpenCV
import OpenCV.Extra.XFeatures2d

transparent, white, black, blue, green, red :: Scalar
transparent = toScalar (V4 255 255 255   0 :: V4 Double)
white       = toScalar (V4 255 255 255 255 :: V4 Double)
black       = toScalar (V4   0   0   0 255 :: V4 Double)
blue        = toScalar (V4 255   0   0 255 :: V4 Double)
green       = toScalar (V4   0 255   0 255 :: V4 Double)
red         = toScalar (V4   0   0 255 255 :: V4 Double)

startDetectAndComputeImg
    :: forall (width    :: Nat)
              (height   :: Nat)
              (channels :: Nat)
              (depth    :: *)
    . Mat ('S [height, width]) channels depth ->
      Mat ('S [height, width]) channels depth

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
