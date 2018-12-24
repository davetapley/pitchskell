module TrackGeometry where
import Prelude hiding (Left, Right)
import Track
import Transform
import Linear.V2
import Data.Vector

relativePosition :: Double -> Double -> Position -> Transform -> Position
relativePosition x y p t = p + (V2 x y `transOn` t)

-- Length equal to track width, travelling left to right
trackUnitVector :: Position
trackUnitVector = V2 1 0

data StraightEdge = LeftEdge { straightEdgeStart :: Position, straightEdgeStop :: Position }
                  | RightEdge { straightEdgeStart :: Position, straightEdgeStop :: Position }


-- Height
-- given: 1.613
-- measured (IRL): 1.58
-- frame: 1.78

straightEdges :: Segment -> Vector StraightEdge
straightEdges (Segment Straight p t)  =
  let left = LeftEdge (relativePosition 0 (-0.5) p t) (relativePosition 1.613 (-0.5) p t)
      right = RightEdge (relativePosition 0 0.5 p t) (relativePosition 1.613 0.5 p t)
  in fromList [left, right]

moveToCircleOrigin :: Segment -> Position
moveToCircleOrigin (Segment Left p t)  = relativePosition 0 (-0.82) p t
moveToCircleOrigin (Segment Right p t)  = relativePosition 0 0.82 p t

moveFromCircleOrigin :: Segment -> Position -> Position
moveFromCircleOrigin (Segment Left _ t) p = relativePosition 0 0.82 p t
moveFromCircleOrigin (Segment Right _ t) p = relativePosition 0 (-0.82) p t

angleFromPoints :: V2 (V2 Double) -> Double
angleFromPoints (V2 (V2 x0 y0) (V2 x1 y1)) =
  let a = atan2 (y1 - y0) (x1 - x0)
  in if a >= 0 then a else (pi*2) + a
