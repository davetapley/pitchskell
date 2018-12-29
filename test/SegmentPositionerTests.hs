module SegmentPositionerTests(tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad
import qualified Data.Vector as V
import Linear
import qualified OpenCV as CV
import Text.Printf

import qualified Loop
import SegmentPositioner
import SegmentPositionerDebug
import qualified Track
import Transform
import MaskDebug
import TrackDebug

import Image

tests :: TestTree
tests = testGroup "Tile positioner tests"
  [ testCase "Inpaint walls" segmentPositionerInpaintWalls
  , testCase "Canny edges" segmentPositionerCanny
  , testCase "Corner radius" segmentPositionerMinRadius
  , testCase "Lines" segmentPositionerLines
  , testCase "Circles" segmentPositionerCircles
  , testCase "Geometry debug" segmentPositionerGeometryDebug
  , testCase "Position stays close-ish" segmentPositionerDistance
  , testCase "Angle is close-ish" segmentPositionerAngle
  , testCase "track" segmentPositionerTrack
  ]

segmentPositionerInpaintWalls :: Assertion
segmentPositionerInpaintWalls = renderImage "/tmp/segmentPositionerInpaintWalls.png" (showInpaintWalls idleNoCars)

segmentPositionerCanny :: Assertion
segmentPositionerCanny = do
  let t = Track.transform idleNoCarsStart
  let edgeImg = showHough t idleNoCars
  renderImage "/tmp/segmentPositionerCannyHough.png" edgeImg
  let edgeImgInpaint = showHough t . inpaintWalls $ idleNoCars
  renderImage "/tmp/segmentPositionerCannyHoughInpaintedWalls.png" edgeImgInpaint

segmentPositionerMinRadius :: Assertion
segmentPositionerMinRadius =
  let t = Track.transform idleNoCarsStart
    in round (innerCornerCircleRadius t) @?= 17

segmentPositionerLines :: Assertion
segmentPositionerLines = V.length (SegmentPositioner.lines idleNoCars) @?= 41

segmentPositionerCircles :: Assertion
segmentPositionerCircles =
  let t = Track.transform idleNoCarsStart
    in V.length (SegmentPositioner.circles t idleNoCars) @?= 56

segmentPositionerGeometryDebug :: Assertion
segmentPositionerGeometryDebug = zipWithM_ image [0 :: Int ..] (Loop.unfold idleNoCarsTrack)
  where image n = renderImage ("/tmp/segmentPositionerGeometryDebug." ++ printf "%02d" n ++ ".png") . positionGeometryDebug idleNoCars

segmentPositionerDistance :: Assertion
segmentPositionerDistance = zipWithM_ (\n s -> closeEnough s @? printf "%02d" n ++ " strayed too far" ) [0 :: Int ..] (Loop.unfold idleNoCarsTrack)
  where closeEnough s = distance (positionTile idleNoCars s ) (Track.position s) < (trackWidth (Track.transform idleNoCarsStart) / 2.0)

segmentPositionerAngle :: Assertion
segmentPositionerAngle = zipWithM_ (\n s -> closeEnough s @? printf "%02d" n ++ " rotated too much" ) [0 :: Int ..] (Loop.unfold idleNoCarsTrack)
  where
    closeEnough s = modDistance (angleFromTransform $ transformTile idleNoCars s ) (angleFromTransform $ Track.transform s) < (pi/4.0)
    modDistance a b = let diff = abs (a - b) in min diff ((2*pi) - diff) -- https://stackoverflow.com/a/6193318/21115

segmentPositionerTrack :: Assertion
segmentPositionerTrack = do
  let positions = positionSegments idleNoCars idleNoCarsTrack
  renderImage "/tmp/segmentPositionerTrackMask.png" $ drawTrackMask idleNoCars positions
  renderImage "/tmp/segmentPositionerTrackOutline.png" $ drawTrackOutline idleNoCars positions

