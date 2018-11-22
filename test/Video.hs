module Video(tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Maybe

import qualified OpenCV as CV
import OpenCV.Core.Types.Mat
import OpenCV.VideoIO.Types

import qualified FrameGrabber
import qualified FrameWriter

tests :: TestTree
tests = testGroup "Video tests"
  [ testCase "Can load" canLoadVideo
  , testCase "Framegrabber" testFrameSizeConsistent
  , testCase "Framewriter id" testFrameWriterId
  ]

video :: FilePath
video = "test/video/idle-no-cars-0-3-frames.mp4"

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
  length infos @?= 3

testFrameWriterId :: Assertion
testFrameWriterId = do
    frames <- FrameGrabber.getFrames video
    FrameWriter.writeFrames "/tmp/frameWriterId.mov" frames
