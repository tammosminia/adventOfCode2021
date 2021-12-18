module Day12
    ( cavePaths1, initCave, paths2, cavePaths2, containsSmallOnce, Cave(Start, Small, Big)
    ) where

import Data.List
import qualified Data.Map as Map
import qualified Data.Char as Char
import Data.List.Split

data Cave = Start | End | Small { name :: String } | Big { name :: String } deriving (Eq, Ord)
type CaveMap = Map.Map Cave [Cave]
type Path = [Cave]

instance Show Cave where
  show Start = "start"
  show End = "end"
  show (Small x) = x
  show (Big x) = x

initCave :: [String] -> CaveMap
initCave lines = mapOfLists $ concatMap parseLine lines
  where
    parseCave "start" = Start
    parseCave "end" = End
    parseCave s
      | Char.isLower (head s) = Small { name = s }
      | otherwise = Big { name = s }
    parseLine line = [(c1, c2), (c2, c1)]
      where [c1, c2] = map parseCave $ splitOn "-" line

cavePaths1 :: [String] -> Int
cavePaths1 i = length $ paths $ initCave i

paths :: CaveMap -> [Path]
paths cm = inner [] Start
  where
    inner path End = [path ++ [End]]
    inner path last = concatMap continue $ filter canContinue $ cm Map.! last
      where
        continue = inner (path ++ [last])
        canContinue :: Cave -> Bool
        canContinue Start = False
        canContinue End = True
        canContinue (Big _) = True
        canContinue Small { name = n } = notElem (Small { name = n }) path

cavePaths2 :: [String] -> Int
cavePaths2 i = length $ paths2 $ initCave i

paths2 :: CaveMap -> [Path]
paths2 cm = inner [] Start
  where
    inner path End = [path ++ [End]]
    inner path last = concatMap continue $ filter canContinue $ cm Map.! last
      where
        continue = inner (path ++ [last])
        canContinue :: Cave -> Bool
        canContinue Start = False
        canContinue End = True
        canContinue (Big _) = True
        canContinue s = (not
        Elem s path) || (containsSmallOnce path)

containsSmallOnce :: Path -> Bool
containsSmallOnce p = all (\x -> (length x) == 1) $ group $ sort $ filter isSmall p
  where
    isSmall Small { name = _ } = True
    isSmall _ = False

mapOfLists :: (Ord k, Eq k) => [(k, v)] -> Map.Map k [v]
mapOfLists l = Map.fromList lll
  where
    ll = groupBy (\(k1, v1) (k2, v2) -> k1 == k2) $ sortOn fst l
    lll = map (\sl -> (fst (head sl), map snd sl)) ll
