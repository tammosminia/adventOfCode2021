module Day18
    ( compileSnailNumber, pathsToNumber, numberToPaths, snailAdd, snailSum1, snailSum2
    ) where

import Util
import Data.List
import Data.List.Split

data SnailNumber = Pair SnailNumber SnailNumber | Single Int deriving (Eq)
data Direction = Left | Right deriving (Show, Eq)
type SnailPath = [Direction] --first is last
type SnailPaths = [(SnailPath, Int)]

reduce :: SnailNumber -> SnailNumber
reduce input = pathsToNumber $ inner $ numberToPaths input
  where
    inner n = case explodeFirst n of
      Nothing -> case splitFirst n of
        Nothing -> n
        Just x -> inner x
      Just x -> inner x

snailAdd :: SnailNumber -> SnailNumber -> SnailNumber
snailAdd n1 n2 = reduce (Pair n1 n2)

compileSnailNumber :: String -> SnailNumber
compileSnailNumber s = n
  where
    (n, "") = parseSnail s

parseSnail :: String -> (SnailNumber, String)
parseSnail ('[' : sr) = ((Pair l r), sr4)
  where
    (l, sr1) = parseSnail sr
    (',' : sr2) = sr1
    (r, sr3) = parseSnail sr2
    (']' : sr4) = sr3
parseSnail (digit : sr) = (Single (Util.charToInt digit), sr)

instance Show SnailNumber where
  show (Single n) = show n
  show (Pair l r) = "[" ++ show l ++ "," ++ show r ++ "]"

numberToPaths :: SnailNumber -> SnailPaths
numberToPaths n = inner [] n
  where
    inner path (Single n) = [(path, n)]
    inner path (Pair l r) = inner (Day18.Left : path) l ++ inner (Day18.Right : path) r
    
pathsToNumber :: SnailPaths -> SnailNumber
pathsToNumber ps = inner []
  where 
    inner current = inner2 $ find (\(p, n) -> p == current) ps
      where 
        inner2 (Just (p, n)) = Single n
        inner2 Nothing = Pair (inner (Day18.Left : current)) (inner (Day18.Right : current))
    
explodeFirst :: SnailPaths -> Maybe SnailPaths
explodeFirst input = explodeFirstPos index
  where
    index = findIndex shouldExplode input
    shouldExplode (path, _) = length path > 4
    explodeFirstPos :: Maybe Int -> Maybe SnailPaths
    explodeFirstPos Nothing = Nothing
    explodeFirstPos (Just leftIndex) = Just u4
      where
        rightIndex = leftIndex + 1
        (leftPath, leftNumber) = input !! leftIndex
        (rightPath, rightNumber) = input !! rightIndex
        pairPath = tail leftPath
        u1 :: SnailPaths
        u1 = if leftIndex == 0 then input else replace input lefterIndex (lefterPath, lefterNumber + leftNumber)
          where
            lefterIndex = leftIndex - 1
            (lefterPath, lefterNumber) = input !! lefterIndex
        u2 :: SnailPaths
        u2 = if rightIndex == length input - 1 then u1 else replace u1 righterIndex (righterPath, righterNumber + rightNumber)
          where
            righterIndex = rightIndex + 1
            (righterPath, righterNumber) = input !! righterIndex
        u3 :: SnailPaths
        u3 = replace u2 leftIndex (pairPath, 0)
        u4 :: SnailPaths
        u4 = delete (rightPath, rightNumber) u3

splitFirst :: SnailPaths -> Maybe SnailPaths
splitFirst input = fmap splitSnail index
  where
    index = findIndex shouldSplit input
    shouldSplit (_, n) = n > 9
    splitSnail :: Int -> SnailPaths
    splitSnail i = beforeI ++ [left, right] ++ afterI
      where
        (path, n) = input !! i
        leftN = n `div` 2
        rightN = n - leftN
        left = ((Day18.Left : path), leftN)
        right = ((Day18.Right : path), rightN)
        beforeI = take i input
        afterI = drop (i + 1) input

snailSum1 :: [String] -> Int
snailSum1 input = magnitude added
  where
    numbers = map compileSnailNumber input
    added = foldl1 snailAdd numbers

magnitude :: SnailNumber -> Int
magnitude (Single i) = i
magnitude (Pair l r) = 3 * magnitude l + 2 * magnitude r

snailSum2 :: [String] -> Int
snailSum2 input = maximum magnitudes
  where
    numbers = map compileSnailNumber input
    magnitudes = map (\(e1, e2) -> magnitude (snailAdd e1 e2)) $ combinations numbers

-- Return all pairs in both directions, not including the same element paired with itself
combinations :: Eq a => [a] -> [(a, a)]
combinations l = concatMap combineWith l
  where
    combineWith e1 = map (\e2 -> (e1, e2)) (delete e1 l)
