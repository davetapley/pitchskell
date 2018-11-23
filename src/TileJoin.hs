module TileJoin where
import Control.Monad
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
findCenter frame = do
  let sift = mkSift defaultSiftParams
  let (keypointsObject, descriptorsObject) = exceptError $ siftDetectAndCompute sift startTile Nothing
  let (keypointsScene, descriptorsScene) = exceptError $ siftDetectAndCompute sift frame Nothing

  fbmatcher <- newFlannBasedMatcher (def { indexParams = FlannKDTreeIndexParams 1 })
  matches <- match fbmatcher descriptorsObject descriptorsScene Nothing

  let (framePts, startPts) =  V.unzip $ V.map (getMatchingPoints keypointsObject keypointsScene) matches
  let homography = exceptError $ findHomography framePts startPts (def { fhpMethod = FindHomographyMethod_RANSAC })
    in case homography of
        Nothing -> pure Nothing
        Just (fm, _) -> pure $ Just $ getVector . fmap (fmap realToFrac . fromPoint) $ perspectiveTransform tilePoints fm

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

startTile :: Mat ('S ['D, 'D]) ('S 4) ('S Word8)
startTile =
    exceptError $ coerceMat $ unsafePerformIO $
      imdecode ImreadUnchanged <$> B.readFile "data/tile-join.png"

tilePoints :: Vector (V2 CDouble)
tilePoints =
  let [h, w] = fmap fromIntegral . miShape . matInfo $ startTile
  in V.fromList [ V2 w 0, V2 w h]

