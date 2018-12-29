module Mask where

import Prelude hiding (Left, Right)

import Control.Monad.Except(MonadError)
import Control.Monad.Primitive
import Data.Int
import Data.Proxy
import qualified Data.Vector as V
import Data.Word
import Linear.V2
import OpenCV

import Colors
import Track
import Transform

type MaskMat = Mat ('S ['D, 'D]) ('S 1) ('S Word8)

mask :: (Int32, Int32) -> Segment -> MaskMat
mask (w, h) segment =
  exceptError $
    withMatM (h ::: w ::: Z) (Proxy :: Proxy (S 1)) (Proxy :: Proxy (S Word8)) black $
      renderMask segment

renderMask
  :: (MonadError CvException m, PrimMonad m)
  => Segment
  -> Mut MaskMat (PrimState m)
  -> m ()

renderMask (Segment Straight p t) imgM =
  let origin = round <$> (p + (V2 0 (0.5) `transOn` t))
      size = round <$> (V2 trackLength 1 `transOn` t)
      points = V.fromList $ map (\pt -> round <$> p + (pt `transOn` t)) [
        V2 0 (-0.5),
        V2 0   0.5,
        V2 trackLength   0.5,
        V2 trackLength (-0.5)]
  in fillConvexPoly imgM points white LineType_AA 0

renderMask (Segment Left p t) imgM =
  let origin = round <$> moveToCircleOrigin (Segment Left p t)
      axis = pure $ round $ outerCornerCircleRadius t :: V2 Int32
      innerRadius = round $ innerCornerCircleRadius t
      V2 x y = trackUnitVector `transOn` t
      angle = atan2 y x / pi * 180
  in do
    ellipse imgM origin axis angle 0 90 white (-1) LineType_8 0
    circle imgM origin innerRadius black (-1) LineType_8 0

renderMask (Segment Right p t) imgM =
  let origin = round <$> moveToCircleOrigin (Segment Right p t)
      axis = pure $ round $ outerCornerCircleRadius t :: V2 Int32
      innerRadius = round $ innerCornerCircleRadius t
      V2 x y = trackUnitVector `transOn` t
      angle = 180 + atan2 y x / pi * 180
  in do
    ellipse imgM origin axis angle 90 180 white (-1) LineType_8 0
    circle imgM origin innerRadius black (-1) LineType_8 0
