{-# LANGUAGE ScopedTypeVariables #-}

module Loop where

import qualified Data.List as L

data Node a = Start a | Node a deriving (Eq, Show)
data Loop a = Loop (Loop a) (Node a) (Loop a)

instance Eq a => Eq (Loop a) where
  a == b = unfold a == unfold b

instance Functor Loop where
  fmap f = mkLoop . fmap f . unfold

instance Show a => Show (Loop a) where
  show = show . unfold

mkLoop :: [a] -> Loop a
mkLoop (x:xs) = let
      first = Loop last (Start x) second
      (second,last) = go first xs first
   in first

 where go :: Loop a -> [a] -> Loop a -> (Loop a, Loop a)
       go prev [] next = (next, prev)
       go prev (x:xs) next = let
         this        = Loop prev (Node x) rest
         (rest,last) = go this xs next
         in  (this,last)

unfold :: forall a. Loop a -> [a]
unfold (Loop _ (Node _) next) = unfold next
unfold (Loop _ (Start start ) next) = start : L.unfoldr nextTilStart next
  where nextTilStart :: Loop a -> Maybe (a, Loop a)
        nextTilStart (Loop _ (Start _) _) = Nothing
        nextTilStart (Loop _ (Node x) rest) = Just (x, rest)

prev :: Loop a -> Loop a
prev (Loop x _ _) = x

next :: Loop a -> Loop a
next (Loop _ _ x) = x

length = L.length . unfold
