module Colors where
import Linear
import OpenCV

transparent, white, black, blue, green, red :: Scalar
transparent = toScalar (V4 255 255 255   0 :: V4 Double)
white       = toScalar (V4 255 255 255 255 :: V4 Double)
black       = toScalar (V4   0   0   0 255 :: V4 Double)
blue        = toScalar (V4 255   0   0 255 :: V4 Double)
green       = toScalar (V4   0 255   0 255 :: V4 Double)
red         = toScalar (V4   0   0 255 255 :: V4 Double)
