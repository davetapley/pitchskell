module TrackTracker where

import Track(Track, Segment(..), Tile(Straight), mkTrack)
import Transform(transformFromVector)
import StartFiducial(findCenter)
import TileMatcher(findTrack)
import SegmentPositioner(positionSegments)
import TrackGeometry(angleFromPoints)

import Data.Word
import Linear.Metric
import Linear.V2
import OpenCV

type FrameMat = Mat ('S ['D, 'D]) ('S 3) ('S Word8)

track :: FrameMat -> IO (Maybe Track)
track frame = do
  mStartVector <- findCenter frame
  case mStartVector of
    Nothing -> return Nothing
    Just startVector -> return $ Just $ mkTrack (mkStart startVector) [Straight]
      -- let track = findTrack frame (mkStart startVector)
      -- in return $ Just $ positionSegments frame track

mkStart :: V2 (V2 Double) -> Segment
mkStart p@(V2 a b) = Segment Straight a (transformFromVector p)
