module Day11
    ( dumboOctopus1, dumboOctopus2, initGrid, step
    ) where

import Data.List

type EnergyLevel = Int
type Grid = [[EnergyLevel]] -- rows, columns. 0,0 is top left
type Location = (Int, Int)

replace :: [a] -> Int -> a -> [a]
replace xs i e = case splitAt i xs of
   (before, _:after) -> before ++ e: after
   _ -> xs
   
charToInt :: Char -> Int
charToInt c = read [c] :: Int

initGrid :: [String] -> Grid
initGrid = map initLine
  where
    initLine = map charToInt

energy :: Grid -> Location -> EnergyLevel
energy g (x, y) = (g !! y) !! x

setEnergy :: Grid -> Location -> EnergyLevel -> Grid
setEnergy g (x, y) e = replace g y $ replace (g !! y) x e

mapGrid :: (EnergyLevel -> EnergyLevel) -> Grid -> Grid
mapGrid f g = map (map f) g

neighbours :: Location -> [Location]
neighbours l = filter inGrid $ map (plus l) deltas
  where
    deltas = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1,1)]
    inGrid (x, y) = between x && between y
    between x = x >= 0 && x <= 9
    
plus :: Location -> Location -> Location
plus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

step :: Grid -> (Grid, Int)
step gridIn = unflash $ flash increased
  where
    allLocations = [(x, y) | x <- [0..9], y <- [0..9]]
    increased = mapGrid (+ 1) gridIn
    findEnergy e g = filter (\l -> energy g l == e) allLocations
    unflash g = (mapGrid elevenToZero g, length (findEnergy 11 g))
    elevenToZero e = if e == 11 then 0 else e
    flash g
      | null tens = g
      | otherwise = flash $ head10to11 $ flashNeighbours
      where 
        tens = findEnergy 10 g
        head10 = head tens
        flashNeighbour g l = setEnergy g l $ newEnergy $ energy g l
        flashNeighbours = foldl flashNeighbour g (neighbours head10)
        head10to11 g = setEnergy g head10 11
        newEnergy x = if x > 9 then x else x + 1
        
dumboOctopus1 :: [String] -> Int -> Int
dumboOctopus1 input steps = inner (initGrid input) steps
  where 
    inner g 0 = 0 
    inner g steps = flashes + (inner ng (steps - 1))
      where  
        (ng, flashes) = step g

dumboOctopus2 :: [String] -> Int
dumboOctopus2 input = inner (initGrid input)
  where
    inner g
      | flashes == 100 = 1
      | otherwise = 1 + inner ng
      where
        (ng, flashes) = step g
