module TrackGeometry where
import Prelude hiding (Left, Right)
import Track
import Linear

relativePosition :: Double -> Double -> Position -> Transform -> Position
relativePosition y x p t = p + (t !* V2 y x)

-- Length equal to track width, travelling left to right
trackUnitVector :: Position
trackUnitVector = V2 1 0

moveToCircleOrigin :: Segment -> Position
moveToCircleOrigin (Segment Right p t)  = relativePosition 0 (-0.82) p t
moveToCircleOrigin (Segment Left p t)  = relativePosition 0 0.82 p t

trackWidth :: Transform -> Double
trackWidth t = norm (t !* trackUnitVector)

innerCornerCircleRadius :: Transform -> Double
innerCornerCircleRadius t = norm (t !* V2 0.32 0)

outerCornerCircleRadius :: Transform -> Double
outerCornerCircleRadius t = norm (t !* V2 1.32 0)
