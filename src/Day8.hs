module Day8
    ( digits1, digits2, parseInputLine, decrypt, decode
    ) where

import Data.List
import qualified Data.Set as Set
import Data.List.Split (splitOn)

type Signal = Char
type Segment = Set.Set Char
type Encoding = [Segment] --[segment x]
type AllSegments = Set.Set Segment --order undetermined
type Output = [Segment] --4 segments

digits1 :: [String] -> Int
digits1 i = length $ filter correctSize $ map length allWords
  where 
    after s = last (splitOn "|" s)
    words s = splitOn " " (after s)
    allWords = concatMap words i
    correctSize 2 = True
    correctSize 3 = True
    correctSize 4 = True
    correctSize 7 = True
    correctSize _ = False
    
parseInputLine :: String -> (AllSegments, Output)
parseInputLine s = (allSegments, output)
  where 
    [before, after] = splitOn " | " s
    parseSegment = Set.fromList
    parseWordString s = map parseSegment $ splitOn " " s
    allSegments = Set.fromList $ parseWordString before
    output = parseWordString after
    
decrypt :: AllSegments -> Encoding
decrypt segments = map digit [0..9]
  where
    find pred = Set.elemAt 0 $ Set.filter pred segments
    digit 0 = find (\x -> (length x == 6) && Set.isSubsetOf (digit 1) x && not (Set.isSubsetOf (digit 4) x))
    digit 1 = find (\x -> length x == 2)
    digit 2 = find (\x -> (length x == 5) && not (Set.isSubsetOf x (digit 6)) && not (Set.isSubsetOf (digit 1) x))
    digit 3 = find (\x -> (length x == 5) && Set.isSubsetOf (digit 1) x)
    digit 4 = find (\x -> length x == 4)
    digit 5 = find (\x -> (length x == 5) && Set.isSubsetOf x (digit 6))
    digit 6 = find (\x -> (length x == 6) && not (Set.isSubsetOf (digit 1) x) && not (Set.isSubsetOf (digit 4) x))
    digit 7 = find (\x -> length x == 3)
    digit 8 = find (\x -> length x == 7)
    digit 9 = find (\x -> (length x == 6) && Set.isSubsetOf (digit 4) x)

decode :: Encoding -> Output -> Int
decode encoding segments = digitsToInt digits
  where
    digitsToInt x = digitsToIntr (reverse x)
    digitsToIntr [x] = x 
    digitsToIntr (h : t) = digitsToIntr t * 10 + h
    digits = map decodeSegment segments
    decodeSegment seg = r
      where 
        Just r = elemIndex seg encoding

digits2 :: [String] -> Int
digits2 is = sum $ map lineResult is
  where 
    lineResult i = decode encoding output
      where
        (allSegments, output) = parseInputLine i
        encoding = decrypt allSegments
        