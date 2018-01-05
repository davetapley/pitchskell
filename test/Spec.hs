import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord
import Data.Maybe

import qualified FrameGrabber
import qualified OpenCV as CV
import OpenCV.Core.Types.Mat
import OpenCV.VideoIO.Types

main :: IO ()
main = defaultMain unitTests


unitTests = testGroup "Unit tests"
  [ testCase "Can load" $ canLoadVideo
  , testCase "Framegrabber" $ testFrameSizeConsistent
  ]

video :: FilePath
video = "test/video/idle-no-cars-0.mov"

canLoadVideo :: Assertion
canLoadVideo = do
    cap <- FrameGrabber.withFile video

    isOpened <- CV.videoCaptureIsOpened cap
    isOpened @?= True

    w <- CV.videoCaptureGetI cap VideoCapPropFrameWidth
    w @?= 720

    canGrab <- CV.videoCaptureGrab cap
    canGrab @?= True

    aFrame <- CV.videoCaptureRetrieve cap
    isJust aFrame @?= True

    (miShape . matInfo $ fromJust aFrame) @?= [480,720]

testFrameSizeConsistent :: Assertion
testFrameSizeConsistent = do
  infos <- FrameGrabber.withFrames video matInfo
  length infos @?= 94

