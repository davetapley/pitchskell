module StartFiducialDebug where
import Control.Monad.Except(MonadError)
import Control.Monad.Primitive
import Data.Int
import Data.Proxy
import Data.Word
import Data.Vector
import Linear
import OpenCV
import OpenCV.Extra.XFeatures2d
import OpenCV.Internal.C.Types
import StartFiducial

transparent, white, black, blue, green, red :: Scalar
transparent = toScalar (V4 255 255 255   0 :: V4 Double)
white       = toScalar (V4 255 255 255 255 :: V4 Double)
black       = toScalar (V4   0   0   0 255 :: V4 Double)
blue        = toScalar (V4 255   0   0 255 :: V4 Double)
green       = toScalar (V4   0 255   0 255 :: V4 Double)
red         = toScalar (V4   0   0 255 255 :: V4 Double)

drawArrow :: FrameMat -> Vector (V2 Double) -> FrameMat
drawArrow frame points = do
  let [h, w] = miShape . matInfo $ frame
    in exceptError $ withMatM (h ::: w ::: Z) (Proxy :: Proxy (S 3)) (Proxy :: Proxy (S Word8))
               white $ drawArrow' frame points
drawArrow'
  :: (MonadError CvException m, PrimMonad m)
  => (Mat ('S '[h, w]) c d)
  -> Vector (V2 Double)
  -> Mut (Mat ('S '[h, w]) c d) (PrimState m)
  -> m ()

drawArrow' frame points imgM = do
  matCopyToM imgM (V2 0 0) frame Nothing
  let a = round <$> points ! 0
  let b = round <$> points ! 1
  arrowedLine imgM a b green 1 LineType_AA 0 0.15

