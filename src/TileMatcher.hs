module TileMatcher where

import Prelude hiding (Left, Right)

import Control.Monad.Except(MonadError)
import Control.Monad.Primitive
import Data.Proxy
import Data.Int
import qualified Data.Vector as V
import Linear.Matrix
import Linear.Vector
import Linear
import Linear.V2
import Linear.V3
import Linear.V4
import OpenCV
import OpenCV.Core.Types
import Data.Word
import Data.List
import Data.Function

import Debug.Trace

import Track
import TrackGeometry
import Loop

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

type MaskMat = Mat ('S ['D, 'D]) ('S 1) ('S Word8)

black :: Scalar
black = toScalar (V4   0   0   0 255 :: V4 Double)

mask :: (Int32, Int32) -> Segment -> MaskMat
mask (w, h) segment =
  exceptError $
    withMatM (h ::: w ::: Z) (Proxy :: Proxy (S 1)) (Proxy :: Proxy (S Word8)) black $
      renderMask segment

white :: Scalar
white = toScalar (V4 255 255 255 255 :: V4 Double)

renderMask
  :: (MonadError CvException m, PrimMonad m)
  => Segment
  -> Mut (Mat ('S '[h, w]) c d) (PrimState m)
  -> m ()

renderMask (Segment Straight p t) imgM =
  let origin = round <$> (p + (t !* V2 0 (-0.5)))
      size = round <$> (t !* V2 1.613 1)
      points = V.fromList $ map (\pt -> round <$> p + (t !* pt)) [
        V2 0 (-0.5),
        V2 0   0.5,
        V2 1.613   0.5,
        V2 1.613 (-0.5)]
  in fillConvexPoly imgM points white LineType_AA 0

renderMask (Segment Left p t) imgM =
  let origin = round <$> moveToCircleOrigin (Segment Left p t)
      axis = round . abs <$> (t !* V2 1.32 1.32)
      innerRadius = round $ distance p (p + (t !* V2 0.32 0))
      V2 x y = (t !* V2 (-1) 0)
      angle = 180 + atan2 y x / pi * 180
  in do
    ellipse imgM origin axis angle 0 90 white (-1) LineType_8 0
    circle imgM origin innerRadius black (-1) LineType_8 0

renderMask (Segment Right p t) imgM =
  let origin = round <$> moveToCircleOrigin (Segment Right p t)
      axis = round . abs <$> (t !* V2 1.32 1.32)
      innerRadius = round $ distance p (p + (t !* V2 0.32 0))
      V2 x y = t !* trackUnitVector
      angle = 180 + atan2 y x / pi * 180
  in do
    ellipse imgM origin axis angle 90 180 white (-1) LineType_8 0
    circle imgM origin innerRadius black (-1) LineType_8 0
