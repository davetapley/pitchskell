module Track where

import Prelude hiding (Left, Right)
import Data.Ratio
import qualified Loop

data Tile = Straight | Left | Right deriving (Eq)
instance Show Tile where
  show Straight = "s"
  show Left = "l"
  show Right = "r"

data Point = Point Rational Rational deriving (Eq, Show)
type Entrance = Point
type Exit = Point
data Segment = Segment Tile Entrance Exit deriving (Eq, Show)
type Track = Loop.Loop Segment

origin = Point 0 0

mkTrack :: [Tile] -> Track
mkTrack tiles = let entrances = origin : scanl nextOrigin origin tiles
                    exits = scanl nextOrigin origin tiles
                in Loop.mkLoop $ zipWith3 Segment tiles entrances exits

nextOrigin :: Point -> Tile -> Point
nextOrigin (Point x y) Straight = Point (x + 1) y
nextOrigin (Point x y) Right = Point (x + 1%2) (y + 1%2)


parseTrack :: String -> Track
parseTrack = mkTrack . map parseTile
  where parseTile t
          | t == 's' = Straight
          | t == 'r' = Right

tile :: Segment -> Tile
tile (Segment t _ _) = t

trackLength :: Track -> Int
trackLength = Loop.length
