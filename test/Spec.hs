import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord
import Data.Maybe

import qualified FrameGrabber
import qualified Track
import qualified OpenCV as CV
import OpenCV.Core.Types.Mat
import OpenCV.VideoIO.Types

main :: IO ()
main = defaultMain unitTests

unitTests = testGroup "Unit tests"
  [ testCase "Can load" $ canLoadVideo
  , testCase "Framegrabber" $ testFrameSizeConsistent
  , trackTests
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

trackTests :: TestTree
trackTests = testGroup "Track tests"
  [ testCase "Track parse to length" $ trackLength
  , testCase "Track loops" $ trackLoops
  ]

testTrack = Track.parseTrack "srrsrr"

trackLength :: Assertion
trackLength = Track.trackLength testTrack  @?= 6

trackLoops :: Assertion
trackLoops = let
  lastSegment = Track.back testTrack
  Track.Segment _ _ exit = lastSegment
  in exit @?= Track.origin

