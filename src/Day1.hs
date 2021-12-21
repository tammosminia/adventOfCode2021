module Day1
    ( countIncreases, slidingIncreases
    ) where

import Util
  
countIncreases :: [Integer] -> Integer
countIncreases [] = 0
countIncreases [h1] = 0
countIncreases (h1 : h2 : t)
  | h1 < h2 = 1 + countIncreases (h2 : t)
  | otherwise = countIncreases (h2 : t)


slidingIncreases :: [Integer] -> Integer
slidingIncreases i =
  countIncreases summed
  where
    windows = slidingWindow 3 i
    summed = map sum windows
