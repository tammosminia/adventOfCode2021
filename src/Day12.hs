module Day12
    ( cavePaths1, initCave, paths2, cavePaths2, containsSmallOnce, canContinueTo
    ) where

import Util
import Data.List
import qualified Data.Map as Map
import qualified Data.Char as Char
import Data.List.Split

type Cave = String
type CaveMap = Map.Map Cave [Cave]
type Path = [Cave]

isSmall :: Cave -> Bool
isSmall "start" = False
isSmall "end" = False
isSmall s = Char.isLower (head s)

initCave :: [String] -> CaveMap
initCave lines = mapOfLists $ concatMap parseLine lines
  where
    parseLine line = [(c1, c2), (c2, c1)]
      where [c1, c2] = splitOn "-" line

cavePaths1 :: [String] -> Int
cavePaths1 i = length $ paths $ initCave i

paths :: CaveMap -> [Path]
paths cm = inner [] "start"
  where
    inner path "end" = [path ++ ["end"]]
    inner path last = concatMap continue $ filter canContinueOn $ cm Map.! last
      where
        continue = inner (path ++ [last])
        canContinueOn :: Cave -> Bool
        canContinueOn "start" = False
        canContinueOn "end" = True
        canContinueOn c
          | isSmall c = notElem c path
          | otherwise = True

cavePaths2 :: [String] -> Int
cavePaths2 i = length $ paths2 $ initCave i

paths2 :: CaveMap -> [Path]
paths2 cm = inner [] "start"
  where
    inner path "end" = [path ++ ["end"]]
    inner path last = concatMap continue $ filter (canContinueTo (path ++ [last])) $ cm Map.! last
      where
        continue = inner (path ++ [last])
        
canContinueTo :: Path -> Cave -> Bool
canContinueTo _ "start" = False
canContinueTo _ "end" = True
canContinueTo path c
   | isSmall c = (notElem c path) || (containsSmallOnce path)
   | otherwise = True

containsSmallOnce :: Path -> Bool
containsSmallOnce p = all (\x -> (length x) == 1) $ group $ sort $ filter isSmall p
