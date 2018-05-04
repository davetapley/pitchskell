{-# LANGUAGE ScopedTypeVariables #-}

module Track where

import Data.List
import Data.Ratio

data WorldPosition = WorldPosition Integer Integer
data LocalPosition = LocalPosition Rational Rational

class LocalPositionable a where
  localPosition :: a -> WorldPosition -> LocalPosition

--instance LocalPositionable Straight where
--  localPosition _ (WorldPosition x y) = LocalPosition (x%1) (y%1)

data Loop a = Node (Loop a) a (Loop a)
mkLoop :: [a] -> Loop a
mkLoop [] = error "must have at least one element"
mkLoop xs = let (first,last) = go last xs first
             in  first

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


data Tile = Straight | Left | Right
type Track = Loop Tile

mkTrack :: [Tile] -> Track
mkTrack = mkLoop
