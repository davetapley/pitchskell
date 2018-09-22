module StartFiducial where
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Except(MonadError)
import Data.Default
import Data.Foldable
import Data.Int
import Data.Proxy
import Data.Vector as V(Vector, fromList, (!), map)
import Data.Word
import Foreign.C.Types
import GHC.Float
import Linear
import OpenCV
import OpenCV.Extra.XFeatures2d
import OpenCV.Internal.C.Types

-- Just for the start tile, no judging.
-- Maybe one day this can be baked with with template Haskell.
import System.IO.Unsafe ( unsafePerformIO )
import qualified Data.ByteString as B

transparent, white, black, blue, green, red :: Scalar
transparent = toScalar (V4 255 255 255   0 :: V4 Double)
white       = toScalar (V4 255 255 255 255 :: V4 Double)
black       = toScalar (V4   0   0   0 255 :: V4 Double)
blue        = toScalar (V4 255   0   0 255 :: V4 Double)
green       = toScalar (V4   0 255   0 255 :: V4 Double)
red         = toScalar (V4   0   0 255 255 :: V4 Double)

drawIt
  :: (MonadError CvException m, PrimMonad m, Foldable f)
  => (Mat ('S '[h, w]) c d)
  -> (f KeyPoint)
  -> Mut (Mat ('S '[h, w]) c d) (PrimState m)
  -> m ()
drawIt frame kpts imgM = do
      void $ matCopyToM imgM (V2 0 0) frame Nothing
      for_ kpts $ \kpt -> do
        let kptRec = keyPointAsRec kpt
        circle imgM (round <$> kptPoint kptRec :: V2 Int32) 5 red 1 LineType_AA 0

sift = mkSift defaultSiftParams

type FrameMat = Mat ('S ['D, 'D]) ('S 3) ('S Word8)

startDetectAndComputeImg
  :: FrameMat
  -> IO (Mat ('S ['D, 'D]) ('S 3) ('S Word8))

startDetectAndComputeImg frame = do
  fbmatcher <- newFlannBasedMatcher (def { indexParams = FlannKDTreeIndexParams 1 })

  -- Not sure if these descriptors are the wrong way around?
  matches :: Vector DMatch <- match fbmatcher (descriptor . siftMat $ frame) (descriptor . siftMat $ startTile)   Nothing

  let (framePts, startPts) = unzip $ toList $ fmap getPoints matches

  case exceptError $ findHomography (fromList framePts) (fromList startPts) def of
    Just (fm, _) -> do
      let corner = perspectiveTransform (V.fromList[ V2 0 0 :: V2 CDouble])  fm
        in putStrLn $ show corner

  let [h, w] = miShape . matInfo $ frame
    in return $ exceptError $ withMatM (h ::: w ::: Z) (Proxy :: Proxy (S 3)) (Proxy :: Proxy (S Word8))
               white $ drawIt frame (keypoints . siftMat $ frame)

  where
    v2ToDouble = fmap (toCDouble . float2Double)
    getPoints dmatch =
      let matchRec = dmatchAsRec dmatch
          queryPt = (keypoints . siftMat $ frame) ! fromIntegral (dmatchQueryIdx matchRec)
          trainPt = (keypoints . siftMat $ startTile) ! fromIntegral (dmatchTrainIdx matchRec)
          queryPtRec = keyPointAsRec queryPt
          trainPtRec = keyPointAsRec trainPt
       in (v2ToDouble $ kptPoint queryPtRec, v2ToDouble $ kptPoint trainPtRec) -- :: (V2 CDouble, V2 CDouble)

type KeyPoints = Vector KeyPoint
type DescriptorMat = Mat 'D 'D 'D

siftMat :: FrameMat -> (KeyPoints, DescriptorMat)
siftMat frame = exceptError $ siftDetectAndCompute sift frame Nothing

keypoints = fst
descriptor = snd

startTile :: Mat ('S ['D, 'D]) ('S 3) ('S Word8)
startTile =
    exceptError $ coerceMat $ unsafePerformIO $
      imdecode ImreadUnchanged <$> B.readFile "data/start-tile.png"
