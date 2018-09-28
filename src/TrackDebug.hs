module TrackDebug where
import Control.Monad as M
import Control.Monad.Except(MonadError)
import Control.Monad.Primitive
import Data.Int
import Data.Proxy
import Data.Word
import Data.Vector hiding (zipWith, map, (++))
import Linear
import OpenCV
import OpenCV.Extra.XFeatures2d
import OpenCV.Internal.C.Types
import Track
import Loop
import qualified Data.Text as T

transparent, white, black, blue, green, red :: Scalar
transparent = toScalar (V4 255 255 255   0 :: V4 Double)
white       = toScalar (V4 255 255 255 255 :: V4 Double)
black       = toScalar (V4   0   0   0 255 :: V4 Double)
blue        = toScalar (V4 255   0   0 255 :: V4 Double)
green       = toScalar (V4   0 255   0 255 :: V4 Double)
red         = toScalar (V4   0   0 255 255 :: V4 Double)

type FrameMat = Mat ('S ['D, 'D]) ('S 3) ('S Word8)

drawTrack :: FrameMat -> Track -> FrameMat
drawTrack frame track = do
  let [h, w] = miShape . matInfo $ frame
    in exceptError $ withMatM (h ::: w ::: Z) (Proxy :: Proxy (S 3)) (Proxy :: Proxy (S Word8))
               white $ do
                 drawSegments frame (Loop.unfold track)

drawSegments
  :: (MonadError CvException m, PrimMonad m)
  => (Mat ('S '[h, w]) c d)
  -> [Segment]
  -> Mut (Mat ('S '[h, w]) c d) (PrimState m)
  -> m ()

drawSegments frame segments imgM = do
  matCopyToM imgM (V2 0 0) frame Nothing
  M.sequence_ $ zipWith (drawSegment imgM) (cycle [red, green, blue]) segments

drawSegment imgM color (Segment _ p (V2 t0 t1)) = do
  let a = round <$> p
  let b = round <$> (p + ((V2 t0 t1) !* V2 1 0))
  arrowedLine imgM a b color 1 LineType_AA 0 0.15
  putText' (showV2 a) a

  putText' (showV2 $ round <$> t0) b
  putText' (showV2 $ round <$> t1) (b + V2 0 10)

  where putText' str pos = putText imgM (T.pack $ str) pos (Font FontHersheySimplex NotSlanted 0.3) color 1 LineType_AA False

showV2 :: Show a => V2 a -> String
showV2 (V2 a b) = show a ++ " " ++ show b
