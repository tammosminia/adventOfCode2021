module Day1
    ( countIncreases, slidingIncreases
    ) where

countIncreases :: [Integer] -> Integer
countIncreases [] = 0
countIncreases [h1] = 0
countIncreases (h1 : h2 : t)
  | h1 < h2 = 1 + countIncreases (h2 : t)
  | otherwise = countIncreases (h2 : t)


slidingWindow n xz@(x:xs)
  | length v < n = []
  | otherwise = v : slidingWindow n xs
  where
    v = take n xz

slidingIncreases :: [Integer] -> Integer
slidingIncreases i =
  countIncreases summed
  where
    windows = slidingWindow 3 i
    summed = map sum windows
