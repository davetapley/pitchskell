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
import TileJoin as TJ
import TileMatcher
import TrackTracker as TT
import qualified TileMatcherDebug
import SegmentPositioner
import SegmentPositionerDebug
import qualified Loop
import qualified Track
import TrackGeometry
import Transform
import qualified OpenCV as CV
import TrackDebug
import OpenCV.Core.Types.Mat
import OpenCV.VideoIO.Types
import qualified Data.Vector as V

import qualified Video
import FrameWriter

import System.IO.Unsafe ( unsafePerformIO )

main :: IO ()
main = defaultMain unitTests

unitTests = testGroup "Unit tests"
  [ Video.tests
  , startFiducialTests
  , tileMatcherTests
  , segmentPositionerTests
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

--idleNoCarsRotated :: CV.Mat ('S ['D, 'D]) ('S 3) ('S Word8)
idleNoCarsRotated =
    CV.exceptError $ coerceMat $ unsafePerformIO $
      CV.imdecode CV.ImreadUnchanged <$> B.readFile "test/images/idle-no-cars-0-rotated.png"

idleNoCarsRotatedStart = Track.Segment Track.Straight (V2 383 487) $ mkTransform (V2 (V2 0 (-57)) (V2 57 0))
idleNoCarsRotatedTrack = fromJust $ Track.parseTrack idleNoCarsRotatedStart "sslrlsllrsslrlls"

renderImage
    :: FilePath
    -> CV.Mat ('CV.S [h, w]) channels depth
    -> IO ()
renderImage fp img = do
    let bs = CV.exceptError $ CV.imencode (CV.OutputPng CV.defaultPngParams) img
    B.writeFile fp bs

testStartFiducialPosition :: Assertion
testStartFiducialPosition = do
  points@(V2 center tip) <- fromJust <$> SF.findCenter idleNoCarsRotated
  renderImage "/tmp/drawCenter.png" $ drawArrow idleNoCarsRotated points

  (round <$> center) @?= V2 383 487
  (round <$> tip) @?= V2 383 430

  points <- fromJust <$> TJ.findCenter idleNoCarsRotated
  renderImage "/tmp/drawCenterTJ.png" $ drawArrow idleNoCarsRotated points

testStartFiducialTransform :: Assertion
testStartFiducialTransform = do
  points <- fromJust <$> SF.findCenter idleNoCarsRotated
  roundTransform (transformFromVector points) @?= Track.transform idleNoCarsRotatedStart

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
  , testCase "Draw track mask" tileMatcherDrawTrackMask
  , testCase "Find track" tileMatcherFindTrack
  ]


tileMatcherStraight :: Assertion
tileMatcherStraight = do
  let straight = idleNoCarsRotatedTrack Loop.!! 1
  renderImage "/tmp/tileMatcherStraight.png" $ TileMatcherDebug.drawTileMasks idleNoCarsRotated straight

tileMatcherLeft :: Assertion
tileMatcherLeft = do
  let left = idleNoCarsRotatedTrack Loop.!! 2
  renderImage "/tmp/tileMatcherLeft.png" $ TileMatcherDebug.drawTileMasks idleNoCarsRotated left

tileMatcherDrawTrackMask :: Assertion
tileMatcherDrawTrackMask =  renderImage "/tmp/tileMatcherTrack.png" $ TileMatcherDebug.drawTrackMask idleNoCarsRotated idleNoCarsRotatedTrack

tileMatcherFindTrack :: Assertion
tileMatcherFindTrack = TileMatcher.findTrack idleNoCarsRotated idleNoCarsRotatedStart @?= idleNoCarsRotatedTrack

segmentPositionerTests :: TestTree
segmentPositionerTests = testGroup "Tile positioner tests"
  [ testCase "Inpaint walls" segmentPositionerInpaintWalls
  , testCase "Canny edges" segmentPositionerCanny
  , testCase "Corner radius" segmentPositionerMinRadius
  , testCase "Lines" segmentPositionerLines
  , testCase "Circles" segmentPositionerCircles
  , testCase "positionStraight" segmentPositionerStraight
  , testCase "positionLeft" segmentPositionerLeft
  , testCase "positionRight" segmentPositionerRight
  , testCase "track" segmentPositionerTrack
  ]

segmentPositionerInpaintWalls :: Assertion
segmentPositionerInpaintWalls = renderImage "/tmp/segmentPositionerInpaintWalls.png" (showInpaintWalls idleNoCarsRotated)

segmentPositionerCanny :: Assertion
segmentPositionerCanny = do
  let t = Track.transform idleNoCarsRotatedStart
  let edgeImg = showHough t idleNoCarsRotated
  renderImage "/tmp/segmentPositionerCannyHough.png" edgeImg
  let edgeImgInpaint = showHough t . inpaintWalls $ idleNoCarsRotated
  renderImage "/tmp/segmentPositionerCannyHoughInpaintedWalls.png" edgeImgInpaint

segmentPositionerMinRadius :: Assertion
segmentPositionerMinRadius =
  let t = Track.transform idleNoCarsRotatedStart
    in round (innerCornerCircleRadius t) @?= 18

segmentPositionerLines :: Assertion
segmentPositionerLines = V.length (SegmentPositioner.lines idleNoCarsRotated) @?= 38

segmentPositionerCircles :: Assertion
segmentPositionerCircles =
  let t = Track.transform idleNoCarsRotatedStart
    in V.length (SegmentPositioner.circles t idleNoCarsRotated) @?= 53

segmentPositionerStraight :: Assertion
segmentPositionerStraight = do
  let straight = idleNoCarsRotatedTrack Loop.!! 1
  distance (positionTile idleNoCarsRotated straight ) (Track.position straight) < trackWidth (Track.transform idleNoCarsRotatedStart) @? "Strayed too far"
  renderImage "/tmp/segmentPositionerStraight.png" $ positionLineDebug idleNoCarsRotated straight

  let straight = idleNoCarsRotatedTrack Loop.!! 5
  renderImage "/tmp/segmentPositionerStraightTwo.png" $ positionLineDebug idleNoCarsRotated straight

  let straight = idleNoCarsRotatedTrack Loop.!! 9
  renderImage "/tmp/segmentPositionerStraightThree.png" $ positionLineDebug idleNoCarsRotated straight

  let straight = idleNoCarsRotatedTrack Loop.!! 10
  renderImage "/tmp/segmentPositionerStraightFour.png" $ positionLineDebug idleNoCarsRotated straight

  let straight = idleNoCarsRotatedTrack Loop.!! 15
  renderImage "/tmp/segmentPositionerStraightFive.png" $ positionLineDebug idleNoCarsRotated straight

segmentPositionerLeft :: Assertion
segmentPositionerLeft = do
  let left = idleNoCarsRotatedTrack Loop.!! 2
  let dist = distance (positionTile idleNoCarsRotated left ) (Track.position left)
  renderImage "/tmp/segmentPositionerLeft.png" $ positionCircleDebug idleNoCarsRotated left
  dist < trackWidth (Track.transform idleNoCarsRotatedStart) @? "Strayed too far"

  let left = idleNoCarsRotatedTrack Loop.!! 4
  renderImage "/tmp/segmentPositionerLeftTwo.png" $ positionCircleDebug idleNoCarsRotated left

segmentPositionerRight :: Assertion
segmentPositionerRight = do
  let right = idleNoCarsRotatedTrack Loop.!! 3
  renderImage "/tmp/segmentPositionerRight.png" $ positionCircleDebug idleNoCarsRotated right
  distance (positionTile idleNoCarsRotated right) (Track.position right) < trackWidth (Track.transform idleNoCarsRotatedStart) @? "Strayed too far"

  let right = idleNoCarsRotatedTrack Loop.!! 8
  renderImage "/tmp/segmentPositionerRightTwo.png" $ positionCircleDebug idleNoCarsRotated right

segmentPositionerTrack :: Assertion
segmentPositionerTrack = do
  let positions = positionSegments idleNoCarsRotated idleNoCarsRotatedTrack
  renderImage "/tmp/segmentPositionerTrackMask.png" $ TileMatcherDebug.drawTrackMask idleNoCarsRotated positions
  renderImage "/tmp/segmentPositionerTrackOutline.png" $ drawTrackOutline idleNoCarsRotated positions

trackDebugTests :: TestTree
trackDebugTests = testGroup "TrackDebug tests"
  [ testCase "drawTrack" trackDebugArrows
  , testCase "outline" trackDebugOutline
  ]

trackDebugArrows :: Assertion
trackDebugArrows = renderImage "/tmp/trackArrows.png" $ drawTrackArrows idleNoCarsRotated idleNoCarsRotatedTrack

trackDebugOutline :: Assertion
trackDebugOutline = renderImage "/tmp/trackOutline.png" $ drawTrackOutline idleNoCarsRotated idleNoCarsRotatedTrack

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
    p @?= V2 1.613 0
    getMat t @?= V2 trackUnitVector (V2 0 1)

trackScanl :: Assertion
trackScanl = let
  [x, y, z] = scanl Track.nextSegment Track.start [Track.Right, Track.Right]
  (Track.Segment x_tile x_p x_t) = x
  (Track.Segment y_tile y_p y_t) = y
  (Track.Segment z_tile z_p z_t) = z
  in do
    x_tile @?= Track.Straight
    x_p @?= zero
    getMat x_t @?= V2 trackUnitVector (V2 0 1)

    y_tile @?= Track.Right
    y_p @?= V2 1.613 0
    getMat y_t @?= V2 trackUnitVector (V2 0 1)

    z_tile @?= Track.Right
    z_p @?= V2 2.433 0.82
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
  [ -- testCase "track" trackTrackingTrack
  -- , testCase "rotated" trackTrackingRotated
  -- , testCase "pertubations" trackTrackingPertubations
  ]

trackTrackingTrack :: Assertion
trackTrackingTrack = do
  track <- fromJust <$> TT.track idleNoCarsRotated
  renderImage "/tmp/trackTrackerOutline.png" $ drawTrackOutline idleNoCarsRotated track

trackTrackingRotated :: Assertion
trackTrackingRotated = do
  (frames :: [FrameGrabber.TestMat]) <- FrameGrabber.getFrames "test/video/idle-no-cars-0.mov"
  outlines <- mapM maybeOutline frames
  FrameWriter.writeFrames "/tmp/trackTrackingRotated.mov" outlines

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
