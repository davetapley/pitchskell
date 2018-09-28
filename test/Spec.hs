import Test.Tasty
import Test.Tasty.HUnit

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
import qualified Loop
import qualified Track
import TrackDebug
import qualified OpenCV as CV
import OpenCV.Core.Types.Mat
import OpenCV.VideoIO.Types
import qualified Data.Vector as V


import System.IO.Unsafe ( unsafePerformIO )

main :: IO ()
main = defaultMain unitTests

unitTests = testGroup "Unit tests"
  [ testCase "Can load" $ canLoadVideo
  , testCase "Framegrabber" $ testFrameSizeConsistent
  -- , startFiducialTests
  , testCase "TrackDebug" $ trackDebugTest
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
  , testCase "Start fiducial consistency" $ testStartFiducialConsistency
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
  let startKeypoints = keypoints $ siftMat $ startTile
  let imgKeypoints = keypoints $ siftMat $ idleNoCarsRotated
  let matches = flannMatches idleNoCarsRotated
  let drawn = CV.exceptError $ CV.drawMatches startTile startKeypoints idleNoCarsRotated imgKeypoints matches def
  renderImage "/tmp/drawMatches.png" drawn

  let (start, img) = matchPairs idleNoCarsRotated matches
  let points = fromJust $ findCenter idleNoCarsRotated
  renderImage "/tmp/drawCenter.png" $ drawArrow idleNoCarsRotated points

  let center = points V.! 0
  let tip = points V.! 1
  (round <$> center) @?= V2 383 487
  (round <$> tip) @?= V2 383 430

testStartFiducialConsistency :: Assertion
testStartFiducialConsistency = do
  (frames :: [FrameGrabber.TestMat]) <- FrameGrabber.getFrames video
  let (points :: [V.Vector (V2 Double)]) = map (fromJust . findCenter) frames

  let (debugs :: [StartFiducial.FrameMat]) = zipWith drawArrow frames points

  let renderFrame n mat = renderImage ("/tmp/testStartFiducial_" ++ show n ++ ".png") mat
  sequence_ $ zipWith renderFrame [0..] debugs

  let centers = fmap (V.! 0) points
  let mean = sumV centers ^/ 3
  let deltas = fmap ((^-^) mean) centers
  (< (V2 1.0 1.0)) <$> deltas @?= replicate 3 True

trackDebugTest :: Assertion
trackDebugTest = do
  let track = fromJust $ Track.parseTrack start "sslrlsllrsslrlls"
      start = Track.Segment Track.Straight (V2 383 487) (V2 (V2 0 (-55)) (V2 (-55) 0))
  renderImage "/tmp/trackDebug.png" $ drawTrack idleNoCarsRotated track

loopTests :: TestTree
loopTests = testGroup "Loop tests"
  [ testCase "mkLoop single" $ mkLoopSingleTest
  , testCase "mkLoop multiple" $ mkLoopMultipleTest
  , testCase "mkLoop multiple second" $ mkLoopMultipleSecondTest
  , testCase "mkLoop multiple last" $ mkLoopMultipleLastTest
  , testCase "loop unfold" $ loopUnfold
  , testCase "loop unfold from middle" $ loopUnfoldMiddle
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
  in do
    one @?= 1
    two @?= 2
    three @?= 3

mkLoopMultipleSecondTest :: Assertion
mkLoopMultipleSecondTest = let
  loop = Loop.mkLoop [1,2,3]
  Loop.Loop _ _ (Loop.Loop left x right) = loop
  Loop.Loop _ (Loop.Start one) _ = left
  Loop.Node two = x
  Loop.Loop _ (Loop.Node three) _ = right
  in do
    one @?= 1
    two @?= 2
    three @?= 3

mkLoopMultipleLastTest :: Assertion
mkLoopMultipleLastTest = let
  loop = Loop.mkLoop [1,2,3]
  Loop.Loop (Loop.Loop left x right) _ _ = loop
  Loop.Loop _ (Loop.Node two) _ = left
  Loop.Node three = x
  Loop.Loop _ (Loop.Start one) _ = right
  in do
    two @?= 2
    three @?= 3
    one @?= 1

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
  [testCase "parseTrack" $ parseTestTrack
  , testCase "parseBadTrack" $ parseBadTrack
  , testCase "start" $ trackStart
  , testCase "nextSegment" $ trackNextSegment
  , testCase "parse to length" $ trackLength
  , testCase "scanl" $ trackScanl
  , testCase "shows" $ trackShow
  , testCase "moves" $ trackMoves
  , testCase "loops" $ trackLoops
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
    p @?= V2 1 0
    t @?= V2 (V2 1 0) (V2 0 1)

trackScanl :: Assertion
trackScanl = let
  x : y : z : [] = scanl Track.nextSegment Track.start [Track.Right, Track.Right]
  (Track.Segment x_tile x_p x_t) = x
  (Track.Segment y_tile y_p y_t) = y
  (Track.Segment z_tile z_p z_t) = z
  in do
    x_tile @?= Track.Straight
    x_p @?= V2 0 0
    x_t @?= V2 (V2 1 0) (V2 0 1)

    y_tile @?= Track.Right
    y_p @?= V2 1 0
    y_t @?= V2 (V2 1 0) (V2 0 1)

    z_tile @?= Track.Right
    z_p @?= V2 2.0 (-1.0)
    z_t @?= V2 (V2 0 1) (V2 (-1) 0)

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
  lengthNub = length . (nubBy ((==) `on` Track.position)) . Loop.unfold
  in lengthNub testTrack @?= Loop.length testTrack

trackLoops :: Assertion
trackLoops = let
  Loop.Loop _ (Loop.Node end) _ = Loop.prev testTrack
  Track.Segment tile p t = end
  in Track.nextSegment end Track.Straight @?= Track.start
