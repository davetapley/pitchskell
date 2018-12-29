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
findCenter = runMaybeT . findStartTileGeometry

findStartTileGeometry :: FrameMat -> MaybeT IO (V2 (V2 Double))
findStartTileGeometry frame = do
  let sift = mkSift defaultSiftParams
  (keypointsObject, descriptorsObject) <- liftMaybe $ safeSiftDetectAndCompute sift startSticker
  (keypointsScene, descriptorsScene) <- liftMaybe $ safeSiftDetectAndCompute sift frame

  fbmatcher <- liftIO $ newFlannBasedMatcher (def { indexParams = FlannKDTreeIndexParams 1 })
  matches <- liftIO $ match fbmatcher descriptorsObject descriptorsScene Nothing

  let (framePoints, startPoints) =  V.unzip $ V.map (getMatchingPoints keypointsObject keypointsScene) matches
  homography <- liftMaybe $ exceptError $ findHomography framePoints startPoints (def { fhpMethod = FindHomographyMethod_RANSAC, fhpRansacReprojThreshold = 1 })

  let framePoints = fmap realToFrac . fromPoint <$> perspectiveTransform stickerPoints (fst homography)
  guard (isRectangle framePoints)

  return $ getStartVectorFromStartSticker framePoints

safeSiftDetectAndCompute :: Sift -> FrameMat -> Maybe (V.Vector KeyPoint, Mat 'D 'D 'D)
safeSiftDetectAndCompute sift mat =
  let result = exceptError $ siftDetectAndCompute sift mat Nothing
  in if V.length (fst result) == 0
       then Nothing
       else Just result

-- From middle of 'bottom', pointing to middle of 'top', having magnitude of track width
getStartVectorFromStartSticker :: Vector (V2 Double) -> V2 (V2 Double)
getStartVectorFromStartSticker vs =
  let bl = vs ! 0
      br = vs ! 1
      tl = vs ! 2
      tr = vs ! 3
      bm = (bl + br) / 2 -- 'bottom-middle'
      tm = (tl + tr) / 2
      origin = bm + ((bm - tm) * 1.8)
      ortho = origin + perp (br - bl)
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

isRectangle :: Vector (V2 Double) -> Bool
isRectangle v =
  let bottom = vDist 0 1
      left = vDist 1 2
      top = vDist 2 3
      right = vDist 3 0
  in similar top bottom && similar left right
      where vDist n m = (v ! n) `distance` (v ! m)
            similar x y = abs(x-y) / ((x+y) / 2) < 0.8
            avg x y = x + y / 2

-- If sticker is 47, then track is 53
startSticker :: Mat ('S ['D, 'D]) ('S 3) ('S Word8)
startSticker =
    exceptError $ coerceMat $ unsafePerformIO $
      imdecode ImreadUnchanged <$> B.readFile "data/start-sticker.png"

stickerPoints :: Vector (V2 CDouble)
stickerPoints =
  let [h, w] = fmap fromIntegral . miShape . matInfo $ startSticker
  in if w > h
    then error "Expect start sticker to be pointing along the X axis"
    -- Along the 'bottom', up the 'left', along the 'top', and back down the 'right'
    else V.fromList [V2 w 0, V2 w h, V2 0 h, V2 0 0]
