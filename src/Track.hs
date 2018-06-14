module Track where

import Prelude hiding (Left, Right)
import Data.Ratio
import qualified Numeric.LinearAlgebra.HMatrix as HM
import qualified Loop

data Tile = Straight | Left | Right deriving (Eq)
instance Show Tile where
  show Straight = "s"
  show Left = "l"
  show Right = "r"

type Position = HM.Vector Double
type Transform = HM.Matrix Double

data Segment = Segment
  { tile :: Tile
  , position :: Position
  , transform ::  Transform
  }deriving (Eq, Show)

type Track = Loop.Loop Segment

start :: Segment
start = Segment Straight (HM.vector [0,0]) (HM.matrix 2 [1,0,0,1])

mkTrack :: [Tile] -> Track
mkTrack (Straight : tiles) = Loop.mkLoop $ scanl nextSegment start tiles
mkTrack _ = error "track must start with a straight"

nextSegment :: Segment -> Tile -> Segment
nextSegment segment tile = Segment tile (exitPosition segment) (exitTransform segment)

exitPosition :: Segment -> Position
exitPosition (Segment tile p t)
  | tile == Straight = p + (t HM.#> HM.vector [1, 0])
  | tile == Left = p + (t HM.#> HM.vector [1, 1])
  | tile == Right = p + (t HM.#> HM.vector [1, -1])

exitTransform :: Segment -> Transform
exitTransform (Segment tile p t)
  | tile == Straight = t
  | tile == Left = t HM.<> HM.matrix 2 [0,1,1,0]
  | tile == Right = t HM.<> HM.matrix 2 [0,1,-1,0]

parseTrack :: String -> Track
parseTrack = mkTrack . map parseTile
  where parseTile t
          | t == 's' = Straight
          | t == 'r' = Right
