module TrackDebug where
import Prelude hiding (Left, Right)
import Control.Monad as M
import Control.Monad.Except(MonadError)
import Control.Monad.Primitive
import Data.Int
import Data.Proxy
import Data.Word
import Data.Vector hiding (zipWith, map, (++))
import qualified Data.Vector as V
import Linear(zero)
import Linear.V2
import OpenCV
import OpenCV.Extra.XFeatures2d
import OpenCV.Internal.C.Types
import Track
import Transform
import Loop
import Colors
import qualified Data.Text as T

type FrameMat = Mat ('S ['D, 'D]) ('S 3) ('S Word8)

drawTrackArrows :: FrameMat -> Track -> FrameMat
drawTrackArrows frame track =
  let [h, w] = miShape . matInfo $ frame
  in exceptError $ withMatM (h ::: w ::: Z) (Proxy :: Proxy (S 3)) (Proxy :: Proxy (S Word8)) white
    $ drawSegmentArrows frame (Loop.unfold track)

drawSegmentArrows
  :: (MonadError CvException m, PrimMonad m)
  => Mat ('S '[h, w]) c d
  -> [Segment]
  -> Mut (Mat ('S '[h, w]) c d) (PrimState m)
  -> m ()

drawSegmentArrows frame segments imgM = do
  matCopyToM imgM zero frame Nothing
  M.zipWithM_ (drawSegmentArrow imgM) (cycle [red, green, blue]) segments

drawSegmentArrow imgM color (Segment _ p t) = do
  let a = round <$> p
  let b = round <$> (p + (trackUnitVector `transOn` t))
  arrowedLine imgM a b color 1 LineType_AA 0 0.15
  putText' (showV2 a) a

  putText' (showV2 b) b
  let (V2 t0 t1) = getMat t
  putText' (showV2 $ round <$> t0) (b + V2 0 10)
  putText' (showV2 $ round <$> t1) (b + V2 0 20)
  putText' (show (round $ angleFromTransform t / (2*pi) * 360)) (b + V2 0 30)

  where putText' str pos = putText imgM (T.pack str) pos (Font FontHersheySimplex NotSlanted 0.3) color 1 LineType_AA False

showV2 :: Show a => V2 a -> String
showV2 (V2 a b) = show a ++ " " ++ show b

drawTrackOutline :: FrameMat -> Track -> FrameMat
drawTrackOutline frame track =
  let [h, w] = miShape . matInfo $ frame
  in exceptError $ withMatM (h ::: w ::: Z) (Proxy :: Proxy (S 3)) (Proxy :: Proxy (S Word8)) white
    $ drawOutlines frame (Loop.unfold track)

drawOutlines
  :: (MonadError CvException m, PrimMonad m)
  => Mat ('S '[h, w]) c d
  -> [Segment]
  -> Mut (Mat ('S '[h, w]) c d) (PrimState m)
  -> m ()

drawOutlines frame segments imgM = do
  matCopyToM imgM zero frame Nothing
  M.mapM_ (drawOutline imgM) (Prelude.zip [0..] segments)

drawOutline
  :: (MonadError CvException m, PrimMonad m)
    => Mut (Mat ('S '[h, w]) c d) (PrimState m)
    -> (Int, Segment)
    -> m ()

drawOutline imgM (n, Segment Straight p t) =
  let origin = round <$> (p + (V2 0 (-0.5) `transOn` t))
      size = round <$> (V2 1.613 1 `transOn` t)
      middle = round <$> p + (V2 (1.613/2.0) 0 `transOn` t)
      points = V.fromList $ map (\pt -> round <$> p + (pt `transOn` t)) [
        V2 0 (-0.5),
        V2 0   0.5,
        V2 1.613   0.5,
        V2 1.613 (-0.5)]
  in do
    line imgM (points ! 0) (points ! 1) green 1 LineType_AA 0
    line imgM (points ! 1) (points ! 2) green 1 LineType_AA 0
    line imgM (points ! 2) (points ! 3) green 1 LineType_AA 0
    line imgM (points ! 3) (points ! 0) green 1 LineType_AA 0
    putText imgM (T.pack (show n)) middle (Font FontHersheySimplex NotSlanted 0.3) green 1 LineType_AA False

drawOutline imgM (n, Segment Left p t) =
  let origin = round <$> moveToCircleOrigin (Segment Left p t)
      outerAxis = round . abs <$> (V2 1.32 1.32 `transOn` t)
      innerAxis = round . abs <$> (V2 0.32 0.32 `transOn` t)
      V2 x y = trackUnitVector `transOn` t
      angle = atan2 y x / pi * 180
      middle = round <$> p + (V2 0.5 0 `transOn` t)
      points = V.fromList $ map (\pt -> round <$> p + (pt `transOn` t)) [
        V2 0 (-0.5),
        V2 0   0.5,
        V2 (1.33)   (-0.83),
        V2 0.33 (-0.83)]
  in do
    ellipse imgM origin outerAxis angle 0 90 green 1 LineType_8 0
    ellipse imgM origin innerAxis angle 0 90 green 1 LineType_8 0
    line imgM (points ! 0) (points ! 1) green 1 LineType_AA 0
    line imgM (points ! 2) (points ! 3) green 1 LineType_AA 0
    putText imgM (T.pack (show n)) middle (Font FontHersheySimplex NotSlanted 0.3) green 1 LineType_AA False

drawOutline imgM (n, Segment Right p t) =
  let origin = round <$> moveToCircleOrigin (Segment Right p t)
      outerAxis = round . abs <$> (V2 1.32 1.32 `transOn` t)
      innerAxis = round . abs <$> (V2 0.32 0.32 `transOn` t)
      V2 x y = trackUnitVector `transOn` t
      angle = 180 + atan2 y x / pi * 180
      middle = round <$> p + (V2 0.5 0 `transOn` t)
      points = V.fromList $ map (\pt -> round <$> p + (pt `transOn` t)) [
        V2 0 (-0.5),
        V2 0   0.5,
        V2 (1.33)   0.83,
        V2 0.33 0.83]
  in do
    ellipse imgM origin outerAxis angle 90 180 green 1 LineType_8 0
    ellipse imgM origin innerAxis angle 90 180 green 1 LineType_8 0
    line imgM (points ! 0) (points ! 1) green 1 LineType_AA 0
    line imgM (points ! 2) (points ! 3) green 1 LineType_AA 0
    putText imgM (T.pack (show n)) middle (Font FontHersheySimplex NotSlanted 0.3) green 1 LineType_AA False
