module Day14
    ( polymer1, polymer3
    ) where

import Util
import Data.List
import qualified Data.Map as Map
import Data.List.Split

type Mer = Char
type Polymer = [Mer]
type MerPair = [Mer]
type Rules = Map.Map MerPair Mer
type Elements = Map.Map Mer Int
type OptiPolyMer = Map.Map MerPair Int --count of all the pairs

polymer1 :: [String] -> Polymer -> Int -> Int
polymer1 rulesIn input steps = maximum elements - minimum elements
  where
    rules = parseRules rulesIn
    inner polymer 0 = polymer
    inner polymer steps = pairInsertion rules $ inner polymer (steps - 1)
    resultPolymer = inner input steps
    elements = map length $ group $ sort resultPolymer
    pairInsertion :: Rules -> Polymer -> Polymer
    pairInsertion rules input = (concatMap (\(x1, x2) -> [x1, x2]) (zip input extra)) ++ [last input]
      where
        extra = map (\x -> rules Map.! x) $ slidingWindow 2 input

parseRules :: [String] -> Rules
parseRules i = Map.fromList $ map parseRule i
  where 
    parseRule s = (from, head to)
      where
        [from, to] = splitOn " -> " s

--failed optimization
polymer2 :: [String] -> Polymer -> Int -> Int
polymer2 rulesIn input steps = maximum elements - minimum elements
  where
    rules = parseRules rulesIn
    inputElements = Map.fromList $ map (\x -> (head x, length x)) $ group $ sort input
    elements = mergeMaps (inputElements : map perPair (slidingWindow 2 input))
    perPair p = inner p steps
    inner :: MerPair -> Int -> Elements -- returns added elements, does not count the pair
    inner [m1, m2] 0 = Map.empty
    inner pair@[m1, m2] steps = mergeMaps [inner [m1, mm] s1, inner [mm, m2] s1, Map.singleton mm 1]
      where
        mm = rules Map.! pair
        s1 = steps - 1


toOpti :: Polymer -> OptiPolyMer
toOpti p = countMap $ slidingWindow 2 p
    
polymer3 :: [String] -> Polymer -> Int -> Int
polymer3 rulesIn input steps = maximum elements - minimum elements
  where
    rules = parseRules rulesIn
    resultOpti = inner (toOpti input) steps
    elements :: Elements
    elements = mergeMaps $ Map.singleton (head input) 1 : map (\([_, m2], n) -> Map.singleton m2 n) (Map.toList resultOpti)
    inner opti 0 = opti
    inner opti steps = inner newOpti (steps - 1)
      where
        newOpti = foldr foldPair Map.empty $ Map.toList opti
        foldPair pa op = mergeMaps [mapPair pa, op]
        mapPair (pair@[m1, m2], amount) = Map.fromList [([m1, middle], amount), ([middle, m2], amount)]
          where
            middle = rules Map.! pair
        
    
    


