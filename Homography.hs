{-# LANGUAGE DataKinds #-}

module Main where
import Control.Monad
import Data.Default
import Data.Foldable
import Data.Int
import Data.Maybe
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

import System.Environment

type DynMat = Mat ('S ['D, 'D]) ('S 3) ('S Word8)


main = do
  -- READ FILES
  let imread = (imdecode ImreadUnchanged <$>) . B.readFile
  [imgObject, imgScene] <- (take 2 <$> getArgs) >>= mapM imread

  -- CREATE DETECTOR
  let sift = mkSift defaultSiftParams

  -- DETECT KEY POINTS AND DESCRIPTORS
  let (keypointsObject, descriptorsObject) = exceptError $ siftDetectAndCompute sift imgObject Nothing
  let (keypointsScene, descriptorsScene) = exceptError $ siftDetectAndCompute sift imgScene Nothing

  -- MATCH
  fbmatcher <- newFlannBasedMatcher (def { indexParams = FlannKDTreeIndexParams 1 })
  matches <- match fbmatcher descriptorsObject descriptorsScene Nothing

  let showDist f = show $ f $ V.map (dmatchDistance . dmatchAsRec) matches
  putStrLn $ "-- Max dist : " ++ showDist maximum
  putStrLn $ "-- Min dist : " ++ showDist minimum
  putStrLn $ "-- Match count : " ++ (show $ V.length matches)

  -- DRAW MATCHES
  let imgMatches = exceptError $ drawMatches imgObject keypointsObject imgScene keypointsScene matches def

  -- FIND HOMOGRAPHY

  let (framePts, startPts) =  V.unzip $ V.map (getMatchingPoints keypointsObject keypointsScene) matches
  let (homography, _) = fromMaybe undefined $ exceptError $ findHomography framePts startPts (def { fhpMethod = FindHomographyMethod_RANSAC })

  let [w, h] = fmap fromIntegral . miShape . matInfo $ imgObject
  let objCorners = V.fromList [ V2 0 0, V2 h 0, V2 h w, V2 0 w]

  let sceneCorners = V.map ((round <$>) . fromPoint . toPoint) $ perspectiveTransform objCorners homography :: V.Vector (V2 Int32)

  -- DRAW HOMOGRAPHY
  let green = toScalar (V4 0 255 0 255 :: V4 Double)
  imgMatchesM <- thaw imgMatches

  let offsetCorner n = (sceneCorners V.! n) + (V2 (round h) 0) -- cheating because we know image is rotated
  let line' n imgM = line imgM (offsetCorner n) (offsetCorner ((n+1) `mod` 4)) green 4 LineType_AA 0
  line' 0 imgMatchesM
  line' 1 imgMatchesM
  line' 2 imgMatchesM
  line' 3 imgMatchesM
  imgMatches <- freeze imgMatchesM

  _:_:f <- getArgs
  case f of
    [] -> do
      withWindow  "Good Matches & Object detection (Haskell)" $ \window -> do
        imshow window imgMatches
        void $ waitKey 0
    [f] -> do
      let bs = exceptError $ imencode (OutputPng defaultPngParams) imgMatches
      B.writeFile f bs

getMatchingPoints :: V.Vector KeyPoint -> V.Vector KeyPoint -> DMatch -> (V2 CDouble, V2 CDouble)
getMatchingPoints keypointsObject keypointsScene dmatch =
  let matchRec = dmatchAsRec dmatch
      queryPt = keypointsObject V.! fromIntegral (dmatchQueryIdx matchRec)
      trainPt = keypointsScene V.! fromIntegral (dmatchTrainIdx matchRec)
      queryPtRec = keyPointAsRec queryPt
      trainPtRec = keyPointAsRec trainPt
     in (v2ToDouble $ kptPoint queryPtRec, v2ToDouble $ kptPoint trainPtRec)
  where v2ToDouble = fmap (toCDouble . float2Double)

