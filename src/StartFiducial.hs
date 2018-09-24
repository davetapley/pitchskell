module StartFiducial where
import Control.Monad
import Data.Default
import Data.Foldable
import Data.Int
import Data.Maybe
import Data.Vector as V(Vector, fromList, (!), map, head, take, unzip)
import qualified Data.Vector as V(map)
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

findCenter :: FrameMat -> Maybe (Vector (V2 Double))
findCenter frame =
  let matches = flannMatches frame
      (framePts, startPts) = matchPairs frame matches
      homography = exceptError $ findHomography framePts startPts (def { fhpMethod = FindHomographyMethod_RANSAC })
    in case homography of
        Nothing -> Nothing
        Just (fm, _) -> Just $ fmap (fmap realToFrac . fromPoint) $ perspectiveTransform tilePoints fm

flannMatches :: FrameMat -> Vector DMatch
flannMatches frame = unsafePerformIO $ do
  fbmatcher <- newFlannBasedMatcher (def { indexParams = FlannKDTreeIndexParams 1 })
  match fbmatcher (siftDescriptor startTile) (siftDescriptor frame) Nothing
    where siftDescriptor =  descriptor . siftMat

matchPairs :: FrameMat -> Vector DMatch -> (Vector (V2 CDouble), Vector (V2 CDouble))
matchPairs frame = V.unzip . V.map (getMatchingPoints frame) -- . V.take 10

getMatchingPoints :: FrameMat -> DMatch -> (V2 CDouble, V2 CDouble)
getMatchingPoints frame dmatch =
  let matchRec = dmatchAsRec dmatch
      queryPt = (keypoints . siftMat $ startTile) ! fromIntegral (dmatchQueryIdx matchRec)
      trainPt = (keypoints . siftMat $ frame) ! fromIntegral (dmatchTrainIdx matchRec)
      queryPtRec = keyPointAsRec queryPt
      trainPtRec = keyPointAsRec trainPt
     in (v2ToDouble $ kptPoint queryPtRec, v2ToDouble $ kptPoint trainPtRec)
  where v2ToDouble = fmap (toCDouble . float2Double)

type KeyPoints = Vector KeyPoint
type DescriptorMat = Mat 'D 'D 'D

siftMat :: FrameMat -> (KeyPoints, DescriptorMat)
siftMat frame =
  let sift = mkSift defaultSiftParams
  in exceptError $ siftDetectAndCompute sift frame Nothing

keypoints = fst
descriptor = snd

startTile :: Mat ('S ['D, 'D]) ('S 3) ('S Word8)
startTile =
    exceptError $ coerceMat $ unsafePerformIO $
      imdecode ImreadUnchanged <$> B.readFile "data/start-tile.png"

tilePoints :: Vector (V2 (CDouble))
tilePoints =
  let [h, w] = fmap fromIntegral . miShape . matInfo $ startTile
  --in V.fromList [V2 (fromIntegral h / 2.0) (fromIntegral w), V2 (fromIntegral h / 2.0)  0]
  in V.fromList
    [ V2 0 0
    , V2 w 0
    , V2 w h
    , V2 0 h
    ]
