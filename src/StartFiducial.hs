module StartFiducial where
import Control.Monad
import Control.Monad.Extra
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Data.Default
import Data.Foldable
import Data.Int
import Data.Maybe
import Data.Vector as V(Vector, fromList, (!), map, head, take, unzip)
import qualified Data.Vector as V
import Data.Word
import Foreign.C.Types
import GHC.Float
import Linear
import OpenCV
import OpenCV.Extra.XFeatures2d
import OpenCV.Internal.C.Types

import System.IO.Unsafe ( unsafePerformIO )
import qualified Data.ByteString as B

type FrameMat = Mat ('S ['D, 'D]) ('S 3) ('S Word8)

findCenter :: FrameMat -> IO (Maybe (V2 (V2 Double)))
findCenter frame = runMaybeT $ do
  let sift = mkSift defaultSiftParams
  (keypointsObject, descriptorsObject) <- liftMaybe $ safeSiftDetectAndCompute sift startTile
  (keypointsScene, descriptorsScene) <- liftMaybe $ safeSiftDetectAndCompute sift frame

  fbmatcher <- liftIO $ newFlannBasedMatcher (def { indexParams = FlannKDTreeIndexParams 1 })
  matches <- liftIO $ match fbmatcher descriptorsObject descriptorsScene Nothing

  let (framePts, startPts) =  V.unzip $ V.map (getMatchingPoints keypointsObject keypointsScene) matches
  homography <- liftMaybe $ exceptError $ findHomography framePts startPts (def { fhpMethod = FindHomographyMethod_RANSAC, fhpRansacReprojThreshold = 1 })
  return $ getVector . fmap (fmap realToFrac . fromPoint) $ perspectiveTransform tilePoints (fst homography)

safeSiftDetectAndCompute :: Sift -> FrameMat -> Maybe (V.Vector KeyPoint, Mat 'D 'D 'D)
safeSiftDetectAndCompute sift mat =
  let result = exceptError $ siftDetectAndCompute sift mat Nothing
  in if V.length (fst result) == 0
       then Nothing
       else Just result

getVector :: Vector (V2 Double) -> V2 (V2 Double)
getVector vs =
  let a = vs ! 0
      b = vs ! 1
      origin = a + ((b - a) / 2)
      ortho = origin + perp (b - a)
    in V2 origin ortho

getMatchingPoints :: V.Vector KeyPoint -> V.Vector KeyPoint -> DMatch -> (V2 CDouble, V2 CDouble)
getMatchingPoints keypointsObject keypointsScene dmatch =
  let getObjectIdx = getMatchingPointIdx keypointsObject dmatchQueryIdx dmatch
      getSceneIdx = getMatchingPointIdx keypointsScene dmatchTrainIdx dmatch
     in (getObjectIdx, getSceneIdx)

getMatchingPointIdx ::  V.Vector KeyPoint -> (DMatchRec -> Int32) -> DMatch -> V2 CDouble
getMatchingPointIdx keypoints f dmatch =
  let matchRec = dmatchAsRec dmatch
      queryPt = keypoints V.! fromIntegral (f matchRec)
      queryPtRec = keyPointAsRec queryPt
     in toCDouble . float2Double <$> kptPoint queryPtRec

startTile :: Mat ('S ['D, 'D]) ('S 3) ('S Word8)
startTile =
    exceptError $ coerceMat $ unsafePerformIO $
      imdecode ImreadUnchanged <$> B.readFile "data/start-tile.png"

tilePoints :: Vector (V2 CDouble)
tilePoints =
  let [h, w] = fmap fromIntegral . miShape . matInfo $ startTile
  in V.fromList [ V2 w 0, V2 w h]
