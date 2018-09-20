{-# LANGUAGE ExistentialQuantification #-}

module Mat where
  import Test.Tasty
  import Test.Tasty.HUnit

  import OpenCV
  import OpenCV.Core.Types.Mat
  import OpenCV.VideoIO.Types
  import OpenCV.Extra.XFeatures2d

  import GHC.Word (Word8)
  import GHC.TypeLits

  import Linear
  import Data.Vector
  import Data.Proxy

  import Data.STRef
  import Control.Monad.ST

  mat1 :: Mat (S '[]) (S 1) (S Word8)
  mat1 = emptyMat

  type Shape = 'S '[('S 100), ('S 200)]
  shape = Proxy :: Proxy Shape

  type Channels = ('S 3)
  channels = Proxy :: Proxy Channels

  type Derpth = ('S Word8)
  depth = Proxy :: Proxy Derpth

  defValue :: Scalar
  defValue = toScalar (V4   0   0   0 255 :: V4 Double)

  --chanId :: forall (a :: DS b) . a -> a
  chanId :: forall (a :: DS b) . Proxy a -> Proxy a
  chanId = id

  mat2 :: Mat Shape Channels Derpth
  mat2 = exceptError $ mkMat shape (chanId channels) depth defValue

  matId :: forall a b c . Mat a b c -> Mat a b c
  matId = id

  siftDerp :: Mat (S [w, h]) b c ->  (Vector KeyPoint, Mat 'D 'D 'D)
  siftDerp frame = exceptError $ siftDetectAndCompute sift frame Nothing
    where sift = mkSift defaultSiftParams

  -- Now do the draw it bit:
  -- matCopyToM
  -- void $ matCopyToM imgM (V2 0 0) frog Nothing
  -- circle imgM (round <$> kptPoint kptRec :: V2 Int32) 5 red 1 LineType_AA 0
  -- and such...

  all :: Assertion
  all =
    let mat1 :: Mat (S '[]) (S 1) (S Word8)
        mat1 = emptyMat

    in matInfo mat2 @?= (matInfo . matId) mat2

