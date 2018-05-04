import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord
import Data.Maybe

import qualified FrameGrabber
import qualified Loop
import qualified Track
import qualified OpenCV as CV
import OpenCV.Core.Types.Mat
import OpenCV.VideoIO.Types

main :: IO ()
main = defaultMain unitTests

unitTests = testGroup "Unit tests"
  [ testCase "Can load" $ canLoadVideo
  , testCase "Framegrabber" $ testFrameSizeConsistent
  , loopTests
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

loopTests :: TestTree
loopTests = testGroup "Track tests"
  [ testCase "mkLoop" $ mkLoopSingleTest
  , testCase "mkLoop" $ mkLoopMultipleTest
  ]

mkLoopSingleTest :: Assertion
mkLoopSingleTest = let
  loop = Loop.mkLoop [1]
  Loop.Loop end (Loop.Start one) next = loop
  in one @?= 1

mkLoopMultipleTest :: Assertion
mkLoopMultipleTest = let
  loop = Loop.mkLoop [1,2,3]
  Loop.Loop end (Loop.Start one) next = loop
  in one @?= 1

trackTests :: TestTree
trackTests = testGroup "Track tests"
  [ testCase "Start" $ trackStart
  , testCase "Track parse to length" $ trackLength
  , testCase "Track loops" $ trackLoops
  ]

testTrack = Track.parseTrack "srrsrr"

trackStart :: Assertion
trackStart = let
  Loop.Loop _ (Loop.Start start) _  = testTrack
  Track.Segment startTile _ _ = start
  in startTile @?= Track.Straight

trackLength :: Assertion
trackLength = Track.trackLength testTrack  @?= 6

trackLoops :: Assertion
trackLoops = let
  Loop.Loop _ (Loop.Node end) _ = Loop.prev testTrack
  Track.Segment _ _ exit = end
  in exit @?= Track.origin

