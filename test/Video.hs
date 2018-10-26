module Video(tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Maybe

import qualified OpenCV as CV
import OpenCV.Core.Types.Mat
import OpenCV.VideoIO.Types

import qualified FrameGrabber

tests :: TestTree
tests = testGroup "Video tests"
  [ testCase "Can load" canLoadVideo
  , testCase "Framegrabber" testFrameSizeConsistent
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

