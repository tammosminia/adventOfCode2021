module Day5
    ( dangerCount1, dangerCount2, crosses2, dangerMap
    ) where

import Data.List (transpose, sortOn, (\\))

type Point = (Int, Int) --x,y 0,0 is top left
type Line = (Point, Point) --includes both ends

crosses1 :: Line -> Point -> Bool
crosses1 ((x1, y1), (x2, y2)) (px, py)
  | x1 == x2 && x1 == px = inside y1 y2 py
  | y1 == y2 && y1 == py = inside x1 x2 px
  | otherwise = False 
      
crosses2 :: Line -> Point -> Bool
crosses2 ((x1, y1), (x2, y2)) (px, py)
  | x1 == x2 && x1 == px = inside y1 y2 py
  | y1 == y2 && y1 == py = inside x1 x2 px
  | x1 == x2 && x1 /= px = False
  | y1 == y2 && y1 /= py = False
  | otherwise = inside x1 x2 px && inside y1 y2 py && (ratio x1 x2 px == ratio y1 y2 py)
    where
      ratio bound1 bound2 middle = (fromIntegral (abs (bound1 - bound2))) / (fromIntegral (abs (middle - bound1)))
      
inside :: Int -> Int -> Int -> Bool
inside bound1 bound2 m = m >= lowerBound && m <= upperBound
  where
    lowerBound = min bound1 bound2
    upperBound = max bound1 bound2

mapSize :: [Line] -> (Int, Int)
mapSize lines = (maximum xs + 1, maximum ys + 1)
  where
    endings = concatMap (\(p1, p2) -> [p1, p2]) lines
    xs = map fst endings
    ys = map snd endings

dangerCount :: (Line -> Point -> Bool) -> [Line] -> Int
dangerCount crosses lines = length $ filter isDangerous allPoints
  where
    (sizeX, sizeY) = mapSize lines
    allPoints = [(x, y) | x <- [0..(sizeX-1)], y <- [0..(sizeY-1)]]
    numCrosses p = length $ filter (`crosses` p) lines
    isDangerous p = numCrosses p > 1

dangerCount1 :: [Line] -> Int
dangerCount1 = dangerCount crosses1

dangerCount2 :: [Line] -> Int
dangerCount2 = dangerCount crosses2

dangerMap :: [Line] -> [[Int]]
dangerMap lines = [dangerLine y | y <- [0..(sizeY-1)]]
  where
    (sizeX, sizeY) = mapSize lines
    numCrosses p = length $ filter (`crosses2` p) lines
    dangerLine y = [numCrosses (x,y) | x <- [0..(sizeX-1)]]


