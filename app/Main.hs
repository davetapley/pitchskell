module Main where

import Lib
import Loop
import Track

import qualified Graphics.Gloss as G

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

scalePoint :: Point -> (Float, Float)
scalePoint (Point x y) = (fromRational x * 100, fromRational y * 100)

tilePath :: Segment -> G.Path
tilePath (Segment _ entrance exit) = [scalePoint entrance, scalePoint exit]

segmentPicture :: G.Color -> Segment -> G.Picture
segmentPicture color = G.Color color . G.line . tilePath

picture :: G.Picture
picture = G.pictures $ zipWith segmentPicture (cycle [G.white, G.red]) (unfold testTrack)
