module Day10
    ( parseLines1, parseLines2
    ) where

import Data.List

data ParseResult = Ok | Corrupt { score :: Int } | Incomplete { open :: OpenBrackets} deriving (Show, Eq)
type OpenBrackets = String --stack of open brackets. head should be closed first

parse :: String -> ParseResult
parse line = internal line ""
  where
    internal "" "" = Ok
    internal "" o = Incomplete { open = o }
    internal (h : t) open
      | h == '(' = internal t (h : open)
      | h == '[' = internal t (h : open)
      | h == '{' = internal t (h : open)
      | h == '<' = internal t (h : open)
      | h == ')' = if openh == '(' then internal t opent else Corrupt {score = 3}
      | h == ']' = if openh == '[' then internal t opent else Corrupt {score = 57}
      | h == '}' = if openh == '{' then internal t opent else Corrupt {score = 1197}
      | h == '>' = if openh == '<' then internal t opent else Corrupt {score = 25137}
      | otherwise = internal t open
      where
        (openh : opent) = open

parseLines1 :: [String] -> Int
parseLines1 input = sum $ map (parseScore . parse) input
  where
    parseScore Corrupt {score = x} = x
    parseScore _ = 0

incompletenessScore Incomplete { open = x } = inner x 0
  where
    inner [] current = current
    inner (h : t) current = inner t ((current * 5) + (braceScore h))
    braceScore '(' = 1
    braceScore '[' = 2
    braceScore '{' = 3
    braceScore '<' = 4
  
parseLines2 :: [String] -> Int
parseLines2 input = middle inCompleteScores
  where
    parseScore Corrupt {score = x} = x
    parseScore _ = 0
    isIncomplete Incomplete { open = _ } = True
    isIncomplete _ = False
    inCompleteScores = sort $ map incompletenessScore $ filter isIncomplete $ map parse input
    middle l = l !! (div (length l - 1) 2)

