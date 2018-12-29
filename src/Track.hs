module Track where

import Prelude hiding (Left, Right)
import Data.Ratio
import Data.Vector(Vector)
import qualified Data.Vector as V
import qualified Data.List.Extended as L
import Text.Printf
import qualified Loop

import Transform

import Linear.Vector
import Linear.V2
import Linear.V3

data Tile = Straight | Left | Right deriving (Eq, Enum)
instance Show Tile where
  show Straight = "s"
  show Left = "l"
  show Right = "r"

type Position = V2 Double

data Segment = Segment
  { tile :: Tile
  , position :: Position
  , transform ::  Transform
  } deriving (Eq)

instance Show Segment where
  show (Segment tile p t) =
    let V2 x y = p
        angle = round $ angleFromTransform t / (2*pi) * 360
      in show tile ++ " " ++ printf "%0.2f" x ++ "×" ++ printf "%0.2f" y ++ " " ++ show angle ++ "°"

type Track = Loop.Loop Segment

mkTrack :: Segment -> [Tile] -> Track
mkTrack start (Straight : tiles) = Loop.mkLoop $ scanl nextSegment start tiles
mkTrack _ _ = error "track must start with a straight"

start :: Segment
start = Segment Straight zero eye

parseTrack :: Segment -> String -> Maybe Track
parseTrack start = (mkTrack start <$>) . mapM parseTile . L.chars
  where parseTile = L.findBy [Straight ..] show

nextSegment :: Segment -> Tile -> Segment
nextSegment segment tile = Segment tile (exitPosition segment) (exitTransform segment)

trackLength = 1.58
cornerWidth = 0.82 -- From origin out to left or right edge

exitPosition :: Segment -> Position
exitPosition (Segment tile p t)
  | tile == Straight = p + (V2 trackLength 0 `transOn` t)
  | tile == Right = p + (V2 cornerWidth cornerWidth `transOn` t)
  | tile == Left = p + (V2 cornerWidth (-cornerWidth) `transOn` t)

exitTransform :: Segment -> Transform
exitTransform (Segment tile p t)
  | tile == Straight = t
  | tile == Right = turnLeft t
  | tile == Left = turnRight t

relativePosition :: Double -> Double -> Position -> Transform -> Position
relativePosition x y p t = p + (V2 x y `transOn` t)

-- Length equal to track width, travelling left to right
trackUnitVector :: Position
trackUnitVector = V2 1 0

data StraightEdge = LeftEdge { straightEdgeStart :: Position, straightEdgeStop :: Position }
                  | RightEdge { straightEdgeStart :: Position, straightEdgeStop :: Position }


angleFromStraightEdge e = angleFromPoints (straightEdgeStart e, straightEdgeStop e)

-- Height
-- given: 1.613
-- measured (IRL): 1.58
-- frame: 1.78

straightEdges :: Segment -> Vector StraightEdge
straightEdges (Segment Straight p t)  =
  let left = LeftEdge (relativePosition 0 (-0.5) p t) (relativePosition 1.613 (-0.5) p t)
      right = RightEdge (relativePosition 0 0.5 p t) (relativePosition 1.613 0.5 p t)
  in V.fromList [left, right]

moveToCircleOrigin :: Segment -> Position
moveToCircleOrigin (Segment Left p t)  = relativePosition 0 (-cornerWidth) p t
moveToCircleOrigin (Segment Right p t)  = relativePosition 0 cornerWidth p t

moveFromCircleOrigin :: Segment -> Position -> Position
moveFromCircleOrigin (Segment Left _ t) p = relativePosition 0 cornerWidth p t
moveFromCircleOrigin (Segment Right _ t) p = relativePosition 0 (-cornerWidth) p t
