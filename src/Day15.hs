module Day15
    ( chiton1, chiton2, cave2
    ) where

import Util
import qualified Grid as Grid
import AStar
import Data.List
import qualified Data.Map as Map
import Data.List.Split

type Risk = Int
type Cave = Grid.Grid Risk

chiton1 :: [String] -> Risk
chiton1 i = riskDistance (0, 0) (Grid.bottomRight cave) cave
  where
    cave = Grid.init i

chiton2 :: [String] -> Risk
chiton2 i = riskDistance (0, 0) (Grid.bottomRight cave) cave
  where
    cave = cave2 i

cave2 :: [String] -> Cave
cave2 i = cave
  where
    cave1 = Grid.init i
    (maxX1, maxY1) = Grid.bottomRight cave1
    sizeX = maxX1 + 1
    sizeY = maxY1 + 1
    cave = [[cavePoint (x, y) | x <- [0..(5 * sizeX - 1)]] | y <- [0..(5 * sizeY - 1)]]
    mod9 x = ((x - 1) `mod` 9) + 1
    cavePoint (x, y) = mod9 (Grid.get cave1 (xm, ym) + xd + yd)
      where 
        (xd, xm) = divMod x sizeX
        (yd, ym) = divMod y sizeY

riskDistance :: Grid.Point -> Grid.Point -> Cave -> Risk
riskDistance from to cave = cost
  where
    Just (cost, path) = astarSearch from (== to) nextNodeFn heuristic
    nextNodeFn p = map (\x -> (x, Grid.get cave x)) $ Grid.neighbours p cave
    (maxX, maxY) = Grid.bottomRight cave
    heuristic (x, y) = maxX - x + maxY - y
          
          
