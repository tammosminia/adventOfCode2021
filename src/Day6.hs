module Day6
    ( lanternfish
    ) where

import Data.List (transpose, sortOn, (\\))
--import qualified Data.Map as Map

type Fish = Int
type InitPopulation = [Fish]
type Population = [Integer] --[Amount of 0 fish, amount of 1 fish,...,amount of 8 fish]

nextDay :: Population -> Population
nextDay p = [fish f | f <- [0..8]]
  where
    fish 8 = p !! 0
    fish 6 = (p !! 0) + (p !! 7)
    fish x = p !! (x + 1)

advanceDays :: Int -> Population -> Population
advanceDays 0 p = p
advanceDays d p = advanceDays (d - 1) (nextDay p)

compilePopulation :: InitPopulation -> Population
compilePopulation p = map countFish [0..8]
  where
    countFish f = toInteger (length $ filter (==f) p)

lanternfish :: Int -> InitPopulation -> Integer
lanternfish d p = count $ advanceDays d (compilePopulation p)

count :: Population -> Integer
count p = sum p