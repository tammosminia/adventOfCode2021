module Util where

import Data.List
import qualified Data.Map as Map

mapOfLists :: (Ord k, Eq k) => [(k, v)] -> Map.Map k [v]
mapOfLists l = Map.fromList lll
  where
    ll = groupBy (\(k1, v1) (k2, v2) -> k1 == k2) $ sortOn fst l
    lll = map (\sl -> (fst (head sl), map snd sl)) ll

countMap :: (Ord k, Eq k) => [k] -> Map.Map k Int
countMap l = Map.fromList $ map count $ group $ sort l
  where count l = (head l, length l)

mergeMaps :: (Ord k, Eq k, Num v) => [Map.Map k v] -> Map.Map k v
mergeMaps = foldr (Map.unionWith (+)) Map.empty

replace :: [a] -> Int -> a -> [a]
replace xs i e = case splitAt i xs of
   (before, _:after) -> before ++ e: after
   _ -> xs
   
charToInt :: Char -> Int
charToInt c = read [c] :: Int

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n xz@(x:xs)
  | length v < n = []
  | otherwise = v : slidingWindow n xs
  where
    v = take n xz
