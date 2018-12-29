module TileMatcher where

import Prelude hiding (Left, Right)

import Control.Monad.Except(MonadError)
import Control.Monad.Primitive
import Data.Proxy
import Data.Int
import qualified Data.Vector as V
import Linear.Vector
import Linear(norm, distance)
import Linear.V2
import Linear.V3
import Linear.V4
import OpenCV
import OpenCV.Core.Types
import Data.Word
import Data.List
import Data.Function

import Debug.Trace

import Mask
import Track
import Transform
import TrackGeometry
import Loop

import Colors

type FrameMat = Mat ('S ['D, 'D]) ('S 3) ('S Word8)

findTrack :: FrameMat -> Segment -> Track
findTrack frame start@(Segment Straight pStart tStart) =
  mkLoop $ take 50 $ unfoldr findNextSegment' (0, start)

  where findNextSegment' (n, segment) =
          if n > 0 && (segment `near` start) then Nothing
            else Just (segment, (n + 1, findNextSegment frame segment))

(Segment _ pStart tStart) `near` (Segment _ p t) = distance pStart p < 10 && t == tStart

findNextSegment :: FrameMat -> Segment -> Segment
findNextSegment frame segment =
  let p = exitPosition segment
      t = exitTransform segment
      tile = findNextTile frame p t
    in Segment tile p t

findNextTile :: FrameMat -> Position -> Transform -> Tile
findNextTile = (((fst . minimumBy (compare `on` snd)) .) .) . candiateTiles

candiateTiles :: FrameMat -> Position -> Transform -> [(Tile, Double)]
candiateTiles frame p t = map (\tile -> (tile, tileOverlap frame (Segment tile p t))) [Straight ..]

tileOverlap :: FrameMat -> Segment -> Double
tileOverlap frame segment =
  let [h, w] = miShape . matInfo $ frame
      segmentMask = Just . mask (w, h)
      mean = fromScalar $ fst $ exceptError $ meanStdDev frame (segmentMask segment)
    in Linear.norm  (mean :: V4 Double)

