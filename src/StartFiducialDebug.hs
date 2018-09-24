module StartFiducialDebug where

import Control.Monad.Except(MonadError)
import Control.Monad.Primitive
import Data.Int
import Data.Proxy
import Data.Word
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

drawPoint :: FrameMat -> V2 Double -> FrameMat
drawPoint frame point = do
  let [h, w] = miShape . matInfo $ frame
    in exceptError $ withMatM (h ::: w ::: Z) (Proxy :: Proxy (S 3)) (Proxy :: Proxy (S Word8))
               white $ drawPoint' frame point
drawPoint'
  :: (MonadError CvException m, PrimMonad m)
  => (Mat ('S '[h, w]) c d)
  -> V2 Double
  -> Mut (Mat ('S '[h, w]) c d) (PrimState m)
  -> m ()
drawPoint' frame point imgM = do
  matCopyToM imgM (V2 0 0) frame Nothing
  let origin = round <$> point
  let tip = origin + V2 20 20
  circle imgM origin 5 red 1 LineType_AA 0
  arrowedLine imgM origin tip blue 2 LineType_AA 0 0.15

