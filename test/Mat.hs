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
  import Data.Int

  import Data.ByteString as B
  import Data.STRef
  import Control.Monad.ST
  import Control.Monad.Primitive

  import System.IO.Unsafe ( unsafePerformIO )

  black :: Scalar
  black = toScalar (V4   0   0   0 255 :: V4 Double)

  white  = toScalar (V4 255 255 255 255 :: V4 Double)

  mat1 :: Mat (S '[]) (S 1) (S Word8)
  mat1 = emptyMat

  type Shape = 'S '[('S 100), ('S 200)]
  shape = Proxy :: Proxy Shape

  type Channels = ('S 3)
  channels = Proxy :: Proxy Channels

  type Derpth = ('S Word8)
  depth = Proxy :: Proxy Derpth

  chanId :: forall (a :: DS b) . Proxy a -> Proxy a
  chanId = id

  mat2 :: Mat Shape Channels Derpth
  mat2 = exceptError $ mkMat shape (chanId channels) depth black

  matId :: forall a b c . Mat a b c -> Mat a b c
  matId = id

  siftDerp :: Mat (S [w, h]) b c ->  (Vector KeyPoint, Mat 'D 'D 'D)
  siftDerp frame = exceptError $ siftDetectAndCompute sift frame Nothing
    where sift = mkSift defaultSiftParams

  font = Font FontHersheySimplex NotSlanted 1.0

  --drawHello :: PrimMonad m => Mut (Mat ('S '[h, w]) c d) (PrimState m) -> m ()
  drawHello imgM =  putText imgM "Hello World"
                      (V2 10 35 :: V2 Int32) font
                      black 1 LineType_AA False

  withMat2 :: forall
                (width    :: Nat)
                (height   :: Nat)
                (channels :: Nat)
                (depth    :: *)
       . (Mat (ShapeT [height, width]) ('S channels) ('S depth) ~ (Mat Shape Channels Derpth))
      => Mat (ShapeT [height, width]) ('S channels) ('S depth)

  withMat2 = exceptError $ do
    withMatM (Proxy :: Proxy [height, width])
             (Proxy :: Proxy channels)
             (Proxy :: Proxy depth)
             white $ drawHello

  drawDerp :: Mat (S [w, h]) b c -> Mat (S [w, h]) b c
  drawDerp = undefined
    --exceptError $ withMatM
    --  (Proxy :: Proxy [w, h])
    --  (Proxy :: Proxy b)
    --  (Proxy :: Proxy c)
    --  defValue $ \imgM -> do
    --    return

  -- Now do the draw it bit:
  -- matCopyToM
  -- void $ matCopyToM imgM (V2 0 0) frog Nothing
  -- circle imgM (round <$> kptPoint kptRec :: V2 Int32) 5 red 1 LineType_AA 0
  -- and such...

  mat3 :: Mat D Channels Derpth
  mat3 = exceptError $ mkMat (fromList [10 :: Int32, 20]) channels depth black

  --withMat3 :: forall
  --              (width    :: Nat)
  --              (height   :: Nat)
  --              (channels :: Nat)
  --              (depth    :: *)
  --     . (Mat (ShapeT [height, width]) ('S channels) ('S depth))
  --    => Mat (ShapeT [height, width]) ('S channels) ('S depth)

  withMat3 = exceptError $ do
    withMatM (fromList [10 :: Int32, 20])
             channels
             depth
             --white $ (\_ -> pure ())
             white $ drawHello

  frog :: Mat D Channels Derpth
  frog =
      exceptError $ coerceMat $ unsafePerformIO $
        imdecode ImreadUnchanged <$> B.readFile "kikker.jpg"

  frogSize :: [Int32]
  frogSize = miShape . matInfo $ frog

  matFrog :: Mat D Channels Derpth
  matFrog = exceptError $ mkMat (fromList frogSize) channels depth black

  all :: Assertion
  all =
    let mat1 :: Mat (S '[]) (S 1) (S Word8)
        mat1 = emptyMat

    in matInfo mat2 @?= (matInfo . matId) mat2

