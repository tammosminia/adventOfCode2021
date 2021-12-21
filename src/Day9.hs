module Day9
    ( lavaTube1, lavaTube2
    ) where

import qualified Grid as Grid
import Data.List
import qualified Data.Set as Set
import Data.List.Split (splitOn)

type Height = Int -- 0 to 9, 0 is lowest
type HeightMap = Grid.Grid Height

localMinima :: HeightMap -> [Grid.Point]
localMinima m = filter isLowest allPoints
  where
    (maxX, maxY) = Grid.bottomRight m
    allPoints = [(x, y) | x <- [0..maxX], y <- [0..maxY]]
    isLowest p = null lowerNeighbours
      where
        localHeight = Grid.get m p
        lowerNeighbours = filter (\pp -> (Grid.get m pp) <= localHeight) (Grid.neighbours p m)
        
lavaTube1 :: [String] -> Int
lavaTube1 input = sum $ map risk $ localMinima m
  where
    m = Grid.init input
    risk p = (Grid.get m p) + 1

basinSize :: HeightMap -> Grid.Point -> Int
basinSize m lmp = inner [] [lmp]
  where 
    inner basin [] = length basin
    inner ps (h : t) = inner (h : ps) (t ++  newPoints)
      where
        newPoints = filter shouldAdd $ Grid.neighbours h m
        shouldAdd p = Grid.get m p < 9 && notElem p ps && notElem p t

lavaTube2 :: [String] -> Int
lavaTube2 input = product $ take 3 $ reverse $ sort basinSizes
  where
    m = Grid.init input
    basinSizes = map (basinSize m) $ localMinima m
