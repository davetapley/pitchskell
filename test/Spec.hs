import Test.Tasty
import Test.Tasty.HUnit

import Control.Lens
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
import StartFiducial as SF
import StartFiducialDebug
import TileMatcher
import TrackTracker as TT
import qualified TileMatcherDebug
import qualified Loop
import qualified Track
import Transform
import qualified OpenCV as CV
import TrackDebug
import OpenCV.Core.Types.Mat
import OpenCV.VideoIO.Types
import qualified Data.Vector as V

import Image
import qualified Video
import MaskDebug
import qualified SegmentPositionerTests
import FrameWriter

import Text.Printf

import System.IO.Unsafe ( unsafePerformIO )

main :: IO ()
main = defaultMain unitTests

unitTests = testGroup "Unit tests"
  [ Video.tests
  , SegmentPositionerTests.tests
  , startFiducialTests
  , tileMatcherTests
  , trackDebugTests
  , loopTests
  , trackTests
  , trackTrackingTests
  ]

startFiducialTests :: TestTree
startFiducialTests = testGroup "Start fiducial tests"
  [ testCase "Start fiducial position" testStartFiducialPosition
  , testCase "Start fiducial transform" testStartFiducialTransform
  , testCase "Start fiducial consistency" testStartFiducialConsistency
  , testStartFiducialIsRectangle
  ]

testStartFiducialPosition :: Assertion
testStartFiducialPosition = do
  points@(V2 center tip) <- fromJust <$> SF.findCenter idleNoCars
  renderImage "/tmp/drawCenter.png" $ drawArrow idleNoCars points

  (round <$> center) @?= V2 485 151
  (round <$> tip) @?= V2 383 430

testStartFiducialTransform :: Assertion
testStartFiducialTransform = do
  points <- fromJust <$> SF.findCenter idleNoCars
  roundTransform (transformFromVector points) @?= Track.transform idleNoCarsStart

video :: FilePath
video = "test/video/idle-no-cars-0-3-frames.mp4"

testStartFiducialConsistency :: Assertion
testStartFiducialConsistency = do
  (frames :: [FrameGrabber.TestMat]) <- FrameGrabber.getFrames video
  (points :: [Maybe (V2 (V2 Double))]) <- mapM SF.findCenter frames

  let (debugs :: [SF.FrameMat]) = zipWith drawArrow frames (map fromJust points)

  let renderFrame n = renderImage ("/tmp/testStartFiducial_" ++ show n ++ ".png")
  zipWithM_ renderFrame [0..] debugs

  let centers = fmap (^._x) (catMaybes points)
  let mean = sumV centers ^/ 3
  let deltas = fmap (mean ^-^) centers
  (< V2 1.0 1.0) <$> deltas @?= replicate 3 True

testStartFiducialIsRectangle :: TestTree
testStartFiducialIsRectangle = testGroup "isRectangle"
  [ testCase "perfect" $ isIt True [V2 5 0, V2 5 10, V2 0 10, V2 0 0]
  , testCase "close" $ isIt True [V2 4 0, V2 5 10, V2 0 13, V2 0 0]
  , testCase "way off" $ isIt False [V2 2 0, V2 5 10, V2 0 12, V2 0 0]
  ]
  where isIt b = (@?= b) . SF.isRectangle . V.fromList

tileMatcherTests :: TestTree
tileMatcherTests = testGroup "Tile matcher tests"
  [ testCase "Straight is a straight" tileMatcherStraight
  , testCase "Left is a left" tileMatcherLeft
  , testCase "Find track" tileMatcherFindTrack
  ]


tileMatcherStraight :: Assertion
tileMatcherStraight = do
  let straight = idleNoCarsTrack Loop.!! 1
  renderImage "/tmp/tileMatcherStraight.png" $ TileMatcherDebug.drawTileMasks idleNoCars straight

tileMatcherLeft :: Assertion
tileMatcherLeft = do
  let left = idleNoCarsTrack Loop.!! 2
  renderImage "/tmp/tileMatcherLeft.png" $ TileMatcherDebug.drawTileMasks idleNoCars left

tileMatcherFindTrack :: Assertion
tileMatcherFindTrack = do
  let foundTrack = TileMatcher.findTrack idleNoCars idleNoCarsStart
  renderImage "/tmp/tileMatcherTrackMask.png" $ drawTrackMask idleNoCars foundTrack
  foundTrack @?= idleNoCarsTrack

trackDebugTests :: TestTree
trackDebugTests = testGroup "TrackDebug tests"
  [ testCase "drawTrack" trackDebugArrows
  , testCase "outline" trackDebugOutline
  , testCase "Draw track mask" trackDrawTrackMask
  ]

trackDebugArrows :: Assertion
trackDebugArrows = renderImage "/tmp/trackArrows.png" $ drawTrackArrows idleNoCars idleNoCarsTrack

trackDebugOutline :: Assertion
trackDebugOutline = renderImage "/tmp/trackOutline.png" $ drawTrackOutline idleNoCars idleNoCarsTrack

trackDrawTrackMask :: Assertion
trackDrawTrackMask =  renderImage "/tmp/trackMask.png" $ drawTrackMask idleNoCars idleNoCarsTrack

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
  , testCase "transform" trackTransform
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
    p @?= V2 Track.trackLength 0
    getMat t @?= V2 Track.trackUnitVector (V2 0 1)

trackScanl :: Assertion
trackScanl = let
  [x, y, z] = scanl Track.nextSegment Track.start [Track.Right, Track.Right]
  (Track.Segment x_tile x_p x_t) = x
  (Track.Segment y_tile y_p y_t) = y
  (Track.Segment z_tile z_p z_t) = z
  in do
    x_tile @?= Track.Straight
    x_p @?= zero
    getMat x_t @?= V2 Track.trackUnitVector (V2 0 1)

    y_tile @?= Track.Right
    y_p @?= V2 Track.trackLength 0
    getMat y_t @?= V2 Track.trackUnitVector (V2 0 1)

    z_tile @?= Track.Right
    z_p @?= V2 (Track.trackLength + Track.cornerWidth) Track.cornerWidth
    getMat z_t @?= V2 (V2 0 1) (V2 (-1) 0)

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
  in show (Track.nextSegment end Track.Straight) @?= show Track.start

trackTransform :: Assertion
trackTransform = do
  transformFromAngle eye 0 @?= mkTransform (V2 (V2 1 0) (V2 0 1))
  (round <$>) <$> getMat (transformFromAngle eye (pi / 2) ) @?= V2 (V2 0 1) (V2 (-1) 0)
  (round <$>) <$> getMat (transformFromAngle eye pi ) @?= V2 (V2 (-1) 0) (V2 0 (-1))
  (round <$>) <$> getMat (transformFromAngle eye (3* (pi/2)) ) @?= V2 (V2 0 (-1)) (V2 1 0)

  Transform.angleFromTransform (mkTransform (V2 (V2 1 0) (V2 0 1))) @?= 0
  Transform.angleFromTransform (mkTransform (V2 (V2 0 1) (V2 (-1) 0))) @?= (pi/2)
  Transform.angleFromTransform (transformFromAngle eye 0) @?= 0
  Transform.angleFromTransform (transformFromAngle eye (pi/2)) @?= (pi/2)
  Transform.angleFromTransform (transformFromAngle eye 2) @?= 2

trackTrackingTests :: TestTree
trackTrackingTests = testGroup "Track tracking"
  [ testCase "track" trackTrackingTrack
  , testCase "rotated" trackTracking
  -- , testCase "pertubations" trackTrackingPertubations
  ]

trackTrackingTrack :: Assertion
trackTrackingTrack = do
  track <- fromJust <$> TT.track idleNoCars
  renderImage "/tmp/trackTrackerOutline.png" $ drawTrackOutline idleNoCars track

trackTracking :: Assertion
trackTracking = do
  (frames :: [FrameGrabber.TestMat]) <- FrameGrabber.getFrames "test/video/idle-no-cars-0-3-frames.mp4"
  outlines <- mapM maybeOutline frames
  FrameWriter.writeFrames "/tmp/trackTracking.mov" outlines

trackTrackingPertubations :: Assertion
trackTrackingPertubations = do
  (frames :: [FrameGrabber.TestMat]) <- FrameGrabber.getFrames "test/video/track/pertubations.mov"
  outlines <- mapM maybeOutline frames
  FrameWriter.writeFrames "/tmp/trackTrackingPertubations.mov" outlines

maybeOutline :: FrameGrabber.TestMat -> IO FrameGrabber.TestMat
maybeOutline frame = do
  maybeTrack <- TT.track frame
  case maybeTrack of
    Nothing -> return frame
    Just track -> return $ drawTrackOutline frame track
