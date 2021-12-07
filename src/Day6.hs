module Day6
    ( lanternfish1
    ) where

import Data.List (transpose, sortOn, (\\))

type Fish = Int
type Population = [Fish]

nextDay :: Population -> Population
nextDay yesterday = ageAllFish ++ newFish
  where
    newFish = [8 | _ <- filter (==0) yesterday]
    ageAllFish = map ageOneFish yesterday
    ageOneFish 0 = 6
    ageOneFish f = f - 1

advanceDays :: Int -> Population -> Population
advanceDays 0 p = p
advanceDays d p = advanceDays (d - 1) (nextDay p)

lanternfish1 :: Int -> Population -> Int
lanternfish1 d p = length $ advanceDays d p
