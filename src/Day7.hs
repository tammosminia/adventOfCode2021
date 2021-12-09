module Day7
    ( crabs, crabs2
    ) where

import Data.List (transpose, sortOn, (\\))
--import qualified Data.Map as Map

type Location = Int
type FuelAmount = Int
type InitPopulation = [Location] --position for each crab
type Population = [Int] --[Amount of crabs on position x]

compilePopulation :: InitPopulation -> Population
compilePopulation p = map countCrabs [0..farthest]
  where 
    farthest = maximum p
    countCrabs l = length $ filter (==l) p

fuelNeeded :: Location -> Population -> FuelAmount
fuelNeeded destination pop = sum $ map forRow [0..(length pop - 1)]
  where
    forRow l = (abs (destination - l)) * (pop !! l)

crabs :: InitPopulation -> FuelAmount
crabs i = minimum $ map (\x -> fuelNeeded x p) [0..(length p - 1)]
  where
    p = compilePopulation i

fuelNeeded2 :: Location -> Population -> FuelAmount
fuelNeeded2 destination pop = sum $ map forRow [0..(length pop - 1)]
  where
    forRow l = crabFuel (abs (destination - l)) * (pop !! l)
    crabFuel 0 = 0
    crabFuel distance = distance + crabFuel (distance - 1)

crabs2 :: InitPopulation -> FuelAmount
crabs2 i = minimum $ map (\x -> fuelNeeded2 x p) [0..(length p - 1)]
  where
    p = compilePopulation i

