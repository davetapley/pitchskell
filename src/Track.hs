module Track where

import Prelude hiding (Left, Right)
import Data.Ratio
import qualified Data.List.Extended as L
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
      in show tile ++ " " ++ show (round x) ++ "×" ++ show (round y) ++ " " ++ show angle ++ "°"

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

exitPosition :: Segment -> Position
exitPosition (Segment tile p t)
  | tile == Straight = p + (V2 1.613 0 `transOn` t)
  | tile == Right = p + (V2 0.82 0.82 `transOn` t)
  | tile == Left = p + (V2 0.82 (-0.82) `transOn` t)

exitTransform :: Segment -> Transform
exitTransform (Segment tile p t)
  | tile == Straight = t
  | tile == Right = turnLeft t
  | tile == Left = turnRight t
