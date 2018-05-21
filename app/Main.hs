module Main where

import Lib
import Loop
import Track
import qualified Numeric.LinearAlgebra.HMatrix as HM

import qualified Graphics.Gloss as G

import GHC.Float

main :: IO ()
main
 = G.display
        (G.InWindow
               "Hello World"     -- window title
                (1000, 1000)     -- window size
                (10, 10))        -- window position
        G.black                  -- background color
        picture                  -- picture to display


testTrack = Track.parseTrack "srrsrr"

scalePoint :: Position -> G.Point
scalePoint p  =
  let [x, y] = HM.toList p
  in (double2Float x * 100, double2Float y * 100)

tilePath :: Segment -> G.Path
tilePath (Segment _ p t) = [scalePoint p, scalePoint (p + (t HM.#> HM.vector [0.5,0.5]))]

segmentPicture :: G.Color -> Segment -> G.Picture
segmentPicture color = G.Color color . G.line . tilePath

segmentLabel :: Show a => G.Color -> Segment -> a -> G.Picture
segmentLabel color (Segment _ p _) a = let
  label = show a ++ " " ++ show p
  translate = (uncurry G.Translate) (scalePoint p)
  scale = G.Scale 0.1 0.1
  in translate $ scale $ G.Color color $ G.Text label

picture :: G.Picture
picture = let
  colors = cycle [G.white, G.red]
  track = unfold testTrack
  lines = zipWith segmentPicture colors track
  labels = zipWith3 segmentLabel colors track (zip track [1..])
  in G.pictures (lines ++ labels)
