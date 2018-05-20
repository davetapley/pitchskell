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
nextSegment (Segment tile p t) Straight =  Segment Straight p t
nextSegment (Segment tile p t) Left = Segment Left p t
nextSegment (Segment tile p t) Right = Segment Right p t

parseTrack :: String -> Track
parseTrack = mkTrack . map parseTile
  where parseTile t
          | t == 's' = Straight
          | t == 'r' = Right
