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

  -- let (framePts, startPts) = matchPairs frame matches
  -- homography = exceptError $ findHomography framePts startPts (def { fhpMethod = FindHomographyMethod_RANSAC })

  _:_:f <- getArgs
  case f of
    [] -> do
      withWindow  "Good Matches & Object detection (Haskell)" $ \window -> do
        imshow window imgMatches
        void $ waitKey 0
    [f] -> do
      let bs = exceptError $ imencode (OutputPng defaultPngParams) imgMatches
      B.writeFile f bs
