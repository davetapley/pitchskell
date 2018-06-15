module Data.List.Extended
  ( module Data.List
  , chars
  , findBy
  ) where

import Data.List

chars :: String -> [String]
chars = map (:[])

findBy :: Eq b => [a] -> (a -> b) -> b -> Maybe a
findBy xs f a = find (\x -> f x == a) xs
