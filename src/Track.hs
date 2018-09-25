module Track where

import Prelude hiding (Left, Right)
import Data.Ratio
import qualified Data.List.Extended as L
import qualified Loop

import Linear.Matrix
import Linear.V2
import Linear.V3

data Tile = Straight | Left | Right deriving (Eq, Enum)
instance Show Tile where
  show Straight = "s"
  show Left = "l"
  show Right = "r"

type Position = V2 Double
type Transform = V2 (V2 Double)

data Segment = Segment
  { tile :: Tile
  , position :: Position
  , transform ::  Transform
  } deriving (Eq)

instance Show Segment where
  show (Segment tile p t) = show tile ++ show p ++ show t

type Track = Loop.Loop Segment

start :: Segment
start = Segment Straight (V2 0 0) (V2 (V2 1 0) (V2 0 1))

mkTrack :: [Tile] -> Track
mkTrack (Straight : tiles) = Loop.mkLoop $ scanl nextSegment start tiles
mkTrack _ = error "track must start with a straight"

parseTrack :: String -> Maybe Track
parseTrack = (mkTrack <$>) . sequence . map parseTile . L.chars
  where parseTile = L.findBy [Straight ..] show

nextSegment :: Segment -> Tile -> Segment
nextSegment segment tile = Segment tile (exitPosition segment) (exitTransform segment)

exitPosition :: Segment -> Position
exitPosition (Segment tile p t)
  | tile == Straight = p + (t !* V2 1 0)
  | tile == Left = p + (t !* V2 1 1)
  | tile == Right = p + (t !* V2 1 (-1))

exitTransform :: Segment -> Transform
exitTransform (Segment tile p t)
  | tile == Straight = t
  | tile == Left = t !*! V2 (V2 0 1) (V2 1 0)
  | tile == Right = t !*! V2 (V2 0 1) (V2 (-1) 0)
