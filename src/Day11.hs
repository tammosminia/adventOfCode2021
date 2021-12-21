module Day11
    ( dumboOctopus1, dumboOctopus2, step
    ) where

import Util
import qualified Grid as Grid
import Data.List

type EnergyLevel = Int
type EGrid = Grid.Grid EnergyLevel

neighbours :: Grid.Point -> [Grid.Point]
neighbours l = filter inGrid $ map (Grid.plus l) deltas
  where
    deltas = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1,1)]
    inGrid (x, y) = between x && between y
    between x = x >= 0 && x <= 9

step :: EGrid -> (EGrid, Int)
step gridIn = unflash $ flash increased
  where
    allLocations = [(x, y) | x <- [0..9], y <- [0..9]]
    increased = Grid.mapGrid (+ 1) gridIn
    findEnergy e g = filter (\l -> Grid.get g l == e) allLocations
    unflash g = (Grid.mapGrid elevenToZero g, length (findEnergy 11 g))
    elevenToZero e = if e == 11 then 0 else e
    flash g
      | null tens = g
      | otherwise = flash $ head10to11 $ flashNeighbours
      where 
        tens = findEnergy 10 g
        head10 = head tens
        flashNeighbour g l = Grid.set g l $ newEnergy $ Grid.get g l
        flashNeighbours = foldl flashNeighbour g (neighbours head10)
        head10to11 g = Grid.set g head10 11
        newEnergy x = if x > 9 then x else x + 1
        
dumboOctopus1 :: [String] -> Int -> Int
dumboOctopus1 input steps = inner (Grid.init input) steps
  where 
    inner g 0 = 0 
    inner g steps = flashes + (inner ng (steps - 1))
      where  
        (ng, flashes) = step g

dumboOctopus2 :: [String] -> Int
dumboOctopus2 input = inner (Grid.init input)
  where
    inner g
      | flashes == 100 = 1
      | otherwise = 1 + inner ng
      where
        (ng, flashes) = step g
