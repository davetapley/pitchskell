module Transform
  ( Transform
  , mkTransform
  , getMat
  , eye
  , trackWidth
  , innerCornerCircleRadius
  , outerCornerCircleRadius
  , angleFromTransform
  , (!*)
  , turnLeft
  , turnRight
  , transformFromAngle
) where

import qualified Linear
import Linear.V2

newtype Transform = Transform (V2 (V2 Double)) deriving (Eq, Show)

mkTransform :: V2 (V2 Double) -> Transform
mkTransform mat =
  if Linear.det22 mat < 0
    then error "Rotation matrix must have positive determinant"
    else Transform mat

getMat (Transform mat) = mat

trackWidth :: Transform -> Double
trackWidth (Transform mat) = Linear.norm (mat Linear.!* V2 1 0)

innerCornerCircleRadius :: Transform -> Double
innerCornerCircleRadius (Transform mat) = Linear.norm (mat Linear.!* V2 0.32 0)

outerCornerCircleRadius :: Transform -> Double
outerCornerCircleRadius (Transform mat) = Linear.norm (mat Linear.!* V2 1.32 0)

angleFromTransform :: Transform -> Double
angleFromTransform (Transform mat) =
  let V2 t_x t_y = (mat Linear.!* V2 1 0)
      a = -atan2 t_y t_x
  in if a >= 0 then a else (pi*2) + a

eye :: Transform
eye = Transform $ V2 (V2 1 0) (V2 0 1)

-- TODO: This is an awful artifact of me messing up rotation matrices
(Transform mat) !* p = p Linear.*! mat

turnLeft (Transform mat) = Transform $ V2 (V2 0 1) (V2 (-1) 0) Linear.!*! mat
turnRight (Transform mat) = Transform $ V2 (V2 0 (-1)) (V2 1 0) Linear.!*! mat

-- clockwise rotation matrix, aka left handed, aka y axes goes down
transformFromAngle :: Transform -> Double -> Transform
transformFromAngle t angle =
      let t' = V2 (V2 (cos angle) (sin angle)) (V2 (-(sin angle)) (cos angle))
      in Transform $ (Linear.^* trackWidth t) <$> t'
