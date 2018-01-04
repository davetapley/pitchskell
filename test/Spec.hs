import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import qualified FrameGrabber
import qualified OpenCV as CV
import OpenCV.VideoIO.Types

main :: IO ()
main = defaultMain unitTests


unitTests = testGroup "Unit tests"
  [ testCase "Framegrabber" $ testFrameSizeConsistent
  ]

testFrameSizeConsistent :: Assertion
testFrameSizeConsistent = do
  widths <- FrameGrabber.withFrames "idle-no-cars-0.mov" getWidth
  (length . nub $ widths) @?= 1
    where getWidth frame = return $ show frame

