module Track where

import Prelude hiding (Left, Right)
import Data.Ratio
import qualified Data.List as L
import qualified Numeric.LinearAlgebra.HMatrix as HM
import qualified Loop

data Tile = Straight | Left | Right deriving (Eq, Enum)
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
  } deriving (Eq)

instance Show Segment where
  show (Segment tile p t) = show tile ++ show p ++ show t

type Track = Loop.Loop Segment

start :: Segment
start = Segment Straight (HM.vector [0,0]) (HM.matrix 2 [1,0,0,1])

mkTrack :: [Tile] -> Track
mkTrack (Straight : tiles) = Loop.mkLoop $ scanl nextSegment start tiles
mkTrack _ = error "track must start with a straight"

findBy :: Eq b => [a] -> (a -> b) -> b -> Maybe a
findBy xs f a = L.find (\x -> f x == a) xs

chars :: String -> [String]
chars = map (:[])

parseTrack :: String -> Maybe Track
parseTrack = (mkTrack <$>) . sequence . map parseTile . chars
  where parseTile = findBy [Straight ..] show

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
