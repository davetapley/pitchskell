module TrackGeometry where
import Prelude hiding (Left, Right)
import Track
import Linear.Matrix
import Linear.V2

circleOrigin :: Segment -> Position
circleOrigin (Segment Right p t)  = realToFrac <$> round <$> (p + (t !* V2 0 (-0.82)))
circleOrigin (Segment Left p t)  = realToFrac <$> round <$> (p + (t !* V2 0 (0.82)))
