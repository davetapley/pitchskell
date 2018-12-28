module Transform
  ( Transform
  , roundTransform
  , mkTransform
  , transformFromVector
  , getMat
  , eye
  , trackWidth
  , innerCornerCircleRadius
  , outerCornerCircleRadius
  , angleFromTransform
  , transOn
  , turnLeft
  , turnRight
  , transformFromAngle
) where

import qualified Linear
import Linear.V2

newtype Transform = Transform (V2 (V2 Double)) deriving (Eq, Show)

roundTransform :: Transform -> Transform
roundTransform (Transform mat) = Transform (((realToFrac . Prelude.round) <$>) <$> mat)

mkTransform :: V2 (V2 Double) -> Transform
mkTransform mat =
  if Linear.det22 mat < 0
    then error "Rotation matrix must have positive determinant"
    else Transform mat

-- clockwise rotation matrix, aka left handed, aka y axes goes down
transformFromVector :: V2 (V2 Double) -> Transform
transformFromVector (V2 a b) =
  let trackWidth = a `Linear.distance` b
      angle = angleFromPoints (a,b)
      t = V2 (V2 (cos angle) (sin angle)) (V2 (-(sin angle)) (cos angle))
  in Transform $ (Linear.^* trackWidth) <$> t

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

transOn :: V2 Double -> Transform -> V2 Double
transOn p (Transform mat) = p Linear.*! mat

turnLeft (Transform mat) = Transform $ V2 (V2 0 1) (V2 (-1) 0) Linear.!*! mat
turnRight (Transform mat) = Transform $ V2 (V2 0 (-1)) (V2 1 0) Linear.!*! mat

-- clockwise rotation matrix, aka left handed, aka y axes goes down
transformFromAngle :: Transform -> Double -> Transform
transformFromAngle t angle =
      let t' = V2 (V2 (cos angle) (sin angle)) (V2 (-(sin angle)) (cos angle))
      in Transform $ (Linear.^* trackWidth t) <$> t'

angleFromPoints :: (V2 Double, V2 Double) -> Double
angleFromPoints (V2 x0 y0, V2 x1 y1) =
  let a = atan2 (y1 - y0) (x1 - x0)
  in if a >= 0 then a else (pi*2) + a
