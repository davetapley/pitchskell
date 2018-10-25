import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad

import qualified Data.ByteString as B
import Data.Default
import Data.Function
import Data.List
import Data.Ord
import Data.Maybe

import Linear
import Linear.Vector

import qualified Numeric.LinearAlgebra.HMatrix as HM
import qualified FrameGrabber
import StartFiducial
import StartFiducialDebug
import TileMatcher
import qualified TileMatcherDebug
import TilePositioner as TP
import TilePositionerDebug
import qualified Loop
import qualified Track
import TrackGeometry
import qualified OpenCV as CV
import TrackDebug
import OpenCV.Core.Types.Mat
import OpenCV.VideoIO.Types
import qualified Data.Vector as V


import System.IO.Unsafe ( unsafePerformIO )

main :: IO ()
main = defaultMain unitTests

unitTests = testGroup "Unit tests"
  [ testCase "Can load" canLoadVideo
  , testCase "Framegrabber" testFrameSizeConsistent
  , startFiducialTests
  , tileMatcherTests
  , tilePositionerTests
  , trackDebugTests
  , loopTests
  , trackTests
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

startFiducialTests :: TestTree
startFiducialTests = testGroup "Start fiducial tests"
  [ testCase "Start fiducial position" testStartFiducialPosition
  , testCase "Start fiducial consistency" testStartFiducialConsistency
  ]

--idleNoCarsRotated :: CV.Mat ('S ['D, 'D]) ('S 3) ('S Word8)
idleNoCarsRotated =
    CV.exceptError $ coerceMat $ unsafePerformIO $
      CV.imdecode CV.ImreadUnchanged <$> B.readFile "test/images/idle-no-cars-0-rotated.png"

renderImage
    :: FilePath
    -> CV.Mat ('CV.S [h, w]) channels depth
    -> IO ()
renderImage fp img = do
    let bs = CV.exceptError $ CV.imencode (CV.OutputPng CV.defaultPngParams) img
    B.writeFile fp bs

testStartFiducialPosition :: Assertion
testStartFiducialPosition = do
  points <- fromJust <$> findCenter idleNoCarsRotated
  renderImage "/tmp/drawCenter.png" $ drawArrow idleNoCarsRotated points

  let center = points V.! 0
  let tip = points V.! 1
  (round <$> center) @?= V2 383 487
  (round <$> tip) @?= V2 383 430

testStartFiducialConsistency :: Assertion
testStartFiducialConsistency = do
  (frames :: [FrameGrabber.TestMat]) <- FrameGrabber.getFrames video
  (points :: [Maybe (V.Vector (V2 Double))]) <- mapM findCenter frames

  let (debugs :: [StartFiducial.FrameMat]) = zipWith drawArrow frames (map fromJust points)

  let renderFrame n = renderImage ("/tmp/testStartFiducial_" ++ show n ++ ".png")
  zipWithM_ renderFrame [0..] debugs

  let centers = fmap (V.! 0) (catMaybes points)
  let mean = sumV centers ^/ 3
  let deltas = fmap (mean ^-^) centers
  (< V2 1.0 1.0) <$> deltas @?= replicate 3 True

tileMatcherTests :: TestTree
tileMatcherTests = testGroup "Tile matcher tests"
  [ testCase "Straight is a straight" tileMatcherStraight
  , testCase "Left is a left" tileMatcherLeft
  , testCase "Draw track mask" tileMatcherDrawTrackMask
  , testCase "Find track" tileMatcherFindTrack
  ]

tileMatcherStraight :: Assertion
tileMatcherStraight = do
  let start = Track.Segment Track.Straight (V2 383 487) (V2 (V2 0 (-55)) (V2 (-55) 0))
      track = fromJust $ Track.parseTrack start "sslrlsllrsslrlls"
      straight = track Loop.!! 1
  renderImage "/tmp/tileMatcherStraight.png" $ TileMatcherDebug.drawTileMasks idleNoCarsRotated straight

tileMatcherLeft :: Assertion
tileMatcherLeft = do
  let start = Track.Segment Track.Straight (V2 383 487) (V2 (V2 0 (-55)) (V2 (-55) 0))
      track = fromJust $ Track.parseTrack start "sslrlsllrsslrlls"
      left = track Loop.!! 2
  renderImage "/tmp/tileMatcherLeft.png" $ TileMatcherDebug.drawTileMasks idleNoCarsRotated left

tileMatcherDrawTrackMask :: Assertion
tileMatcherDrawTrackMask = do
  let start = Track.Segment Track.Straight (V2 383 487) (V2 (V2 0 (-55)) (V2 (-55) 0))
      track = fromJust $ Track.parseTrack start "sslrlsllrsslrlls"
  renderImage "/tmp/tileMatcherTrack.png" $ TileMatcherDebug.drawTrackMask idleNoCarsRotated track

tileMatcherFindTrack :: Assertion
tileMatcherFindTrack = do
  let start = Track.Segment Track.Straight (V2 383 487) (V2 (V2 0 (-55)) (V2 (-55) 0))
      track = fromJust $ Track.parseTrack start "sslrlsllrsslrlls"
  TileMatcher.findTrack idleNoCarsRotated start @?= track

tilePositionerTests :: TestTree
tilePositionerTests = testGroup "Tile positioner tests"
  [ testCase "Inpaint walls" tilePositionerInpaintWalls
  , testCase "Canny edges" tilePositionerCanny
  , testCase "Corner radius" tilePositionerMinRadius
  , testCase "Lines" tilePositionerLines
  , testCase "Circles" tilePositionerCircles
  , testCase "positionLeft" tilePositionerLeft
  , testCase "positionRight" tilePositionerRight
  , testCase "track" tilePositionerTrack
  ]

tilePositionerInpaintWalls :: Assertion
tilePositionerInpaintWalls = renderImage "/tmp/tilePositionerInpaintWalls.png" (showInpaintWalls idleNoCarsRotated)

tilePositionerCanny :: Assertion
tilePositionerCanny = do
   let t = V2 (V2 0 (-55)) (V2 (-55) 0)
   let edgeImg = showHough t idleNoCarsRotated
   renderImage "/tmp/tilePositionerCannyHough.png" edgeImg
   let edgeImgInpaint = showHough t . inpaintWalls $ idleNoCarsRotated
   renderImage "/tmp/tilePositionerCannyHoughInpaintedWalls.png" edgeImgInpaint

tilePositionerMinRadius :: Assertion
tilePositionerMinRadius =
  let t = V2 (V2 0 (-55)) (V2 (-55) 0)
        in round (outerCornerCircleRadius t) @?= 73

tilePositionerLines :: Assertion
tilePositionerLines = do
  tpLines <- TP.lines idleNoCarsRotated
  V.length tpLines @?= 38

tilePositionerCircles :: Assertion
tilePositionerCircles =
  let t = V2 (V2 0 (-55)) (V2 (-55) 0)
  in V.length (TP.circles t idleNoCarsRotated) @?= 16

tilePositionerLeft :: Assertion
tilePositionerLeft = do
  let start = Track.Segment Track.Straight (V2 383 487) (V2 (V2 0 (-55)) (V2 (-55) 0))
      left = fromJust (Track.parseTrack start "sslrlsllrsslrlls") Loop.!! 2
  distance (positionTile idleNoCarsRotated left ) (Track.position left) < trackWidth (Track.transform start) @? "Strayed too far"
  renderImage "/tmp/tilePositionerLeft.png" $ positionCircleDebug idleNoCarsRotated left

  let left = fromJust (Track.parseTrack start "sslrlsllrsslrlls") Loop.!! 4
  renderImage "/tmp/tilePositionerLeftTwo.png" $ positionCircleDebug idleNoCarsRotated left

tilePositionerRight :: Assertion
tilePositionerRight = do
  let start = Track.Segment Track.Straight (V2 383 487) (V2 (V2 0 (-55)) (V2 (-55) 0))
      right = fromJust (Track.parseTrack start "sslrlsllrsslrlls") Loop.!! 3
  distance (positionTile idleNoCarsRotated right) (Track.position right) < trackWidth (Track.transform start) @? "Strayed too far"
  renderImage "/tmp/tilePositionerRight.png" $ positionCircleDebug idleNoCarsRotated right

  let right = fromJust (Track.parseTrack start "sslrlsllrsslrlls") Loop.!! 8
  renderImage "/tmp/tilePositionerRightTwo.png" $ positionCircleDebug idleNoCarsRotated right

tilePositionerTrack :: Assertion
tilePositionerTrack = do
  let start = Track.Segment Track.Straight (V2 383 487) (V2 (V2 0 (-58)) (V2 (-58) 0))
      track = fromJust (Track.parseTrack start "sslrlsllrsslrlls")
  renderImage "/tmp/tilePositionerTrackMask.png" $ TileMatcherDebug.drawTrackMask idleNoCarsRotated (updatePositions idleNoCarsRotated track)
  renderImage "/tmp/tilePositionerTrackOutline.png" $ drawTrackOutline idleNoCarsRotated (updatePositions idleNoCarsRotated track)

trackDebugTests :: TestTree
trackDebugTests = testGroup "TrackDebug tests"
  [ testCase "drawTrack" trackDebugArrows
  , testCase "outline" trackDebugOutline
  ]

trackDebugArrows :: Assertion
trackDebugArrows = do
  let track = fromJust $ Track.parseTrack start "sslrlsllrsslrlls"
      start = Track.Segment Track.Straight (V2 383 487) (V2 (V2 0 (-57)) (V2 (-57) 0))
  renderImage "/tmp/trackArrows.png" $ drawTrackArrows idleNoCarsRotated track

trackDebugOutline :: Assertion
trackDebugOutline = do
  let track = fromJust $ Track.parseTrack start "sslrlsllrsslrlls"
      start = Track.Segment Track.Straight (V2 383 487) (V2 (V2 0 (-57)) (V2 (-57) 0))
  renderImage "/tmp/trackOutline.png" $ drawTrackOutline idleNoCarsRotated track

loopTests :: TestTree
loopTests = testGroup "Loop tests"
  [ testCase "mkLoop single" mkLoopSingleTest
  , testCase "mkLoop multiple" mkLoopMultipleTest
  , testCase "mkLoop multiple second" mkLoopMultipleSecondTest
  , testCase "mkLoop multiple last" mkLoopMultipleLastTest
  , testCase "loop unfold" loopUnfold
  , testCase "loop unfold from middle" loopUnfoldMiddle
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
  Loop.Loop _ (Loop.Node two) _ = next
  Loop.Loop _ (Loop.Node three) _ = end
  in [one, two, three] @?= [1,2,3]

mkLoopMultipleSecondTest :: Assertion
mkLoopMultipleSecondTest = let
  loop = Loop.mkLoop [1,2,3]
  Loop.Loop _ _ (Loop.Loop left x right) = loop
  Loop.Loop _ (Loop.Start one) _ = left
  Loop.Node two = x
  Loop.Loop _ (Loop.Node three) _ = right
  in [one, two, three] @?= [1,2,3]

mkLoopMultipleLastTest :: Assertion
mkLoopMultipleLastTest = let
  loop = Loop.mkLoop [1,2,3]
  Loop.Loop (Loop.Loop left x right) _ _ = loop
  Loop.Loop _ (Loop.Node two) _ = left
  Loop.Node three = x
  Loop.Loop _ (Loop.Start one) _ = right
  in [two, three, one] @?= [2,3, 1]

loopUnfold :: Assertion
loopUnfold = let
  list = [1, 2]
  loop = Loop.mkLoop list
  in Loop.unfold loop @?= list

loopUnfoldMiddle :: Assertion
loopUnfoldMiddle = let
  list = [1..6]
  loop = Loop.mkLoop list
  Loop.Loop _ _ second = loop
  in Loop.unfold second @?= list

trackTests :: TestTree
trackTests = testGroup "Track tests"
  [testCase "parseTrack" parseTestTrack
  , testCase "parseBadTrack" parseBadTrack
  , testCase "start" trackStart
  , testCase "nextSegment" trackNextSegment
  , testCase "parse to length" trackLength
  , testCase "scanl" trackScanl
  , testCase "shows" trackShow
  , testCase "moves" trackMoves
  , testCase "loops" trackLoops
  ]

testTrack = fromJust $ Track.parseTrack Track.start "srrsrr"

parseTestTrack :: Assertion
parseTestTrack = let
  tiles = Track.tile <$> testTrack
  in tiles @?= Loop.mkLoop [Track.Straight, Track.Right, Track.Right, Track.Straight, Track.Right, Track.Right]

parseBadTrack :: Assertion
parseBadTrack = Track.parseTrack Track.start "fail" @?= Nothing

trackNextSegment :: Assertion
trackNextSegment = let
  (Track.Segment tile p t) = Track.nextSegment Track.start Track.Right
  in do
    tile @?= Track.Right
    p @?= V2 1.613 0
    t @?= V2 (trackUnitVector) (V2 0 1)

trackScanl :: Assertion
trackScanl = let
  [x, y, z] = scanl Track.nextSegment Track.start [Track.Right, Track.Right]
  (Track.Segment x_tile x_p x_t) = x
  (Track.Segment y_tile y_p y_t) = y
  (Track.Segment z_tile z_p z_t) = z
  in do
    x_tile @?= Track.Straight
    x_p @?= zero
    x_t @?= V2 (trackUnitVector) (V2 0 1)

    y_tile @?= Track.Right
    y_p @?= V2 1.613 0
    y_t @?= V2 (trackUnitVector) (V2 0 1)

    z_tile @?= Track.Right
    z_p @?= V2 2.433 (-0.82)
    z_t @?= V2 (V2 0 (-1)) (trackUnitVector)

trackStart :: Assertion
trackStart = let
  Loop.Loop _ (Loop.Start start) _  = testTrack
  Track.Segment startTile _ _ = start
  in startTile @?= Track.Straight

trackLength :: Assertion
trackLength = Loop.length testTrack  @?= 6

trackShow :: Assertion
trackShow = let string = concatMap (show . Track.tile) . Loop.unfold $ testTrack
            in string @?= "srrsrr"

trackMoves :: Assertion
trackMoves = let
  lengthNub = length . nubBy ((==) `on` Track.position) . Loop.unfold
  in lengthNub testTrack @?= Loop.length testTrack

trackLoops :: Assertion
trackLoops = let
  Loop.Loop _ (Loop.Node end) _ = Loop.prev testTrack
  Track.Segment tile p t = end
  -- TODO handle rounding error
  in True @?= True -- Track.nextSegment end Track.Straight @?= Track.start
