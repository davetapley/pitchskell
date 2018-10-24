module TrackGeometry where
import Prelude hiding (Left, Right)
import Track
import Linear

relativePosition :: Double -> Double -> Position -> Transform -> Position
relativePosition y x p t = p + (t !* V2 y x)

moveToCircleOrigin :: Segment -> Position
moveToCircleOrigin (Segment Right p t)  = relativePosition 0 (-0.7) p t
moveToCircleOrigin (Segment Left p t)  = relativePosition 0 0.7 p t

moveFromCircleOrigin :: Segment -> Position -> Position
moveFromCircleOrigin (Segment Right _ t) p = relativePosition 0 0.7 p t
moveFromCircleOrigin (Segment Left _ t) p = relativePosition 0 (-0.7) p t

trackWidth :: Transform -> Double
trackWidth t = distance (V2 0 0) (t !* V2 1 0)

outerCornerCircleRadius :: Transform -> Double
outerCornerCircleRadius t = distance (V2 0 0) (t !* V2 1.3 0)
