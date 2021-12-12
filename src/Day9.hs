module Day9
    ( lavaTube1, lavaTube2
    ) where

import Data.List
import qualified Data.Set as Set
import Data.List.Split (splitOn)

type Height = Int -- 0 to 9, 0 is lowest
type HeightMap = [[Height]] --list of rows, top to bottom, left to right
type Point = (Int, Int) -- x,y

parseHeightMap :: [String] -> HeightMap
parseHeightMap = map parseRow
  where
    parseRow = map parsePoint
    parsePoint c = read [c] :: Int
    
pointHeight :: Point -> HeightMap -> Height    
pointHeight (x, y) m = (m !! y) !! x

bottomRight :: HeightMap -> Point
bottomRight m = (length (head m) - 1, length m - 1)

neighbours :: Point -> HeightMap -> [Point]
neighbours (x, y) m = left ++ right ++ top ++ bottom
  where
    (maxX, maxY) = bottomRight m
    left = if x > 0 then [(x - 1, y)] else []
    right = if x < maxX then [(x + 1, y)] else []
    top = if y > 0 then [(x, y - 1)] else []
    bottom = if y < maxY then [(x, y + 1)] else []

localMinima :: HeightMap -> [Point]
localMinima m = filter isLowest allPoints
  where
    (maxX, maxY) = bottomRight m
    allPoints = [(x, y) | x <- [0..maxX], y <- [0..maxY]]
    isLowest p = null lowerNeighbours
      where
        localHeight = pointHeight p m
        lowerNeighbours = filter (\pp -> (pointHeight pp m) <= localHeight) (neighbours p m)
        
lavaTube1 :: [String] -> Int
lavaTube1 input = sum $ map risk $ localMinima m
  where
    m = parseHeightMap input
    risk p = (pointHeight p m) + 1

basinSize :: HeightMap -> Point -> Int
basinSize m lmp = inner [] [lmp]
  where 
    inner basin [] = length basin
    inner ps (h : t) = inner (h : ps) (t ++  newPoints)
      where
        newPoints = filter shouldAdd $ neighbours h m
        shouldAdd p = pointHeight p m < 9 && notElem p ps && notElem p t

lavaTube2 :: [String] -> Int
lavaTube2 input = product $ take 3 $ reverse $ sort basinSizes
  where
    m = parseHeightMap input
    basinSizes = map (basinSize m) $ localMinima m
