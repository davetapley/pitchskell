module Track where

import Prelude hiding (Left, Right)
import Data.Ratio
import qualified Loop

data Tile = Straight | Left | Right deriving (Eq, Show)
data Point = Point Rational Rational deriving (Eq, Show)
type Entrance = Point
type Exit = Point
data Segment = Segment Tile Entrance Exit deriving Eq
type Track = Loop.Loop Segment

origin = Point 0 0

mkTrack :: [Tile] -> Track
mkTrack tiles = let entrances = scanr nextOrigin origin tiles
                    exits = tail entrances ++ [head entrances]
                in Loop.mkLoop $ zipWith3 Segment tiles entrances exits

  where nextOrigin :: Tile -> Point -> Point
        nextOrigin Straight (Point x y) = Point (x + 1) y
        nextOrigin Right (Point x y) = Point (x + 1%2) (y + 1%2)


parseTrack :: String -> Track
parseTrack = mkTrack . map parseTile
  where parseTile t
          | t == 's' = Straight
          | t == 'r' = Right

trackLength :: Track -> Int
trackLength = Loop.length
