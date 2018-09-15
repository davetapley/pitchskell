{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module StartFiducial where

import Control.Monad
import Data.Foldable
import Data.Int
import Data.Proxy
import Data.Word
import GHC.TypeLits
import Linear
import OpenCV
import OpenCV.Extra.XFeatures2d
import Data.ByteString as B
import System.IO.Unsafe ( unsafePerformIO )

transparent, white, black, blue, green, red :: Scalar
transparent = toScalar (V4 255 255 255   0 :: V4 Double)
white       = toScalar (V4 255 255 255 255 :: V4 Double)
black       = toScalar (V4   0   0   0 255 :: V4 Double)
blue        = toScalar (V4 255   0   0 255 :: V4 Double)
green       = toScalar (V4   0 255   0 255 :: V4 Double)
red         = toScalar (V4   0   0 255 255 :: V4 Double)

type Frog             = Mat (ShapeT [ 390,  500]) ('S 3) ('S Word8)

frog :: Frog
frog =
    exceptError $ coerceMat $ unsafePerformIO $
      imdecode ImreadUnchanged <$> B.readFile "kikker.jpg"

-- inspired by https://github.com/LumiGuide/haskell-opencv/blob/5fe41ffe54bf850a65aeef8a507753ade61a44cf/opencv-extra/src/OpenCV/Extra/XFeatures2d.hs#L295
startDetectAndComputeImg :: forall
                (width    :: Nat)
                (height   :: Nat)
                (channels :: Nat)
                (depth    :: *)
       . (Mat (ShapeT [height, width]) ('S channels) ('S depth) ~ Frog)
      => Mat (ShapeT [height, width]) ('S channels) ('S depth)

startDetectAndComputeImg = exceptError $ do
    (kpts, _descs) <- siftDetectAndCompute sift frog Nothing
    withMatM (Proxy :: Proxy [height, width])
             (Proxy :: Proxy channels)
             (Proxy :: Proxy depth)
             white $ \imgM -> do
      void $ matCopyToM imgM (V2 0 0) frog Nothing
      for_ kpts $ \kpt -> do
        let kptRec = keyPointAsRec kpt
        circle imgM (round <$> kptPoint kptRec :: V2 Int32) 5 red 1 LineType_AA 0
  where
    sift = mkSift defaultSiftParams
