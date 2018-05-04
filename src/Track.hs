{-# LANGUAGE ScopedTypeVariables #-}

module Track where

import Prelude hiding (Left, Right)
import Data.List
import Data.Ratio

data Loop a = Node (Loop a) a (Loop a)


mkLoop :: [a] -> Loop a
mkLoop [] = error "must have at least one element"
mkLoop xs = let (start, end) = go end xs start
            in start

  where go :: Loop a -> [a] -> Loop a -> (Loop a, Loop a)
        go prev []     next = (next,prev)
        go prev (x:xs) next = let this        = Node prev x rest
                                  (rest,last) = go this xs next
                              in  (this,last)

unfoldLoop :: forall a. Eq a => Loop a -> [a]
unfoldLoop (Node _ start rest) = start : unfoldr nextTilStart rest
  where nextTilStart :: Loop a -> Maybe (a, Loop a)
        nextTilStart (Node _ x rest')
          | x == start = Nothing
          | otherwise  = Just (x, rest')

loopSize :: Eq a => Loop a -> Int
loopSize = length . unfoldLoop

loopPrev :: Loop a -> a
loopPrev (Node (Node _ x _) _ _) = x

data Tile = Straight | Left | Right deriving Eq
data Point = Point Rational Rational deriving (Eq, Show)
type Entrance = Point
type Exit = Point
data Segment = Segment Tile Entrance Exit deriving Eq
type Track = Loop Segment


origin = Point 0 0

mkTrack :: [Tile] -> Track
mkTrack tiles = let entrances = scanr nextOrigin origin tiles
                    exits = tail entrances ++ [head entrances]
                in mkLoop $ zipWith3 Segment tiles entrances exits

  where nextOrigin :: Tile -> Point -> Point
        nextOrigin Straight (Point x y) = Point (x + 1) y
        nextOrigin Right (Point x y) = Point (x + 1%2) (y + 1%2)


parseTrack :: String -> Track
parseTrack = mkTrack . map parseTile
  where parseTile t
          | t == 's' = Straight
          | t == 'r' = Right

trackLength :: Track -> Int
trackLength = loopSize

back :: Track -> Segment
back = loopPrev
