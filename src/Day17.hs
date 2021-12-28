module Day17
    ( trickShot1, hitsTarget, trickShot2
    ) where

import Util
import Data.List
import Data.List.Split

type Point = (Int, Int) --(0, inf) = top left, y goes up
type Area = (Point, Point) -- (topLeft, bottomRight)

trickShot1 :: Int -> Area -> Int
trickShot1 topSpeed target = maximum $ map highestPoint $ map snd hittingShots
  where
    ((tx1, ty1), (tx2, ty2)) = target
    allShots = [(x, y) | x <- [0..tx2], y <- [0..topSpeed]]
    hittingShots = filter (`hitsTarget` target) allShots

trickShot2 :: Int -> Area -> Int
trickShot2 topSpeed target = length hittingShots
  where
    ((tx1, ty1), (tx2, ty2)) = target
    allShots = [(x, y) | x <- [0..tx2], y <- [ty2..topSpeed]]
    hittingShots = filter (`hitsTarget` target) allShots

hitsTarget :: (Int, Int) -> Area -> Bool
hitsTarget speed target = inner (0, 0) speed
  where
    ((tx1, ty1), (tx2, ty2)) = target
    drag 0 = 0
    drag x = (signum x) * ((abs x) - 1)
    inner (x, y) (sx, sy)
      | y < ty2 = False
      | inside (x, y) target = True
      | otherwise = inner (x + sx, y + sy) (drag sx, sy - 1)

highestPoint :: Int -> Int
highestPoint sy = inner 0 sy
  where
    inner y 0 = y
    inner y sy = inner (y + sy) (sy - 1)


inside :: Point -> Area -> Bool
inside (x, y) ((x1, y1), (x2, y2)) = x >= x1 && x <= x2 && y <= y1 && y >= y2
