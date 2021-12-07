module Day4
    ( bestScore, worstScore
    ) where

import Data.List (transpose, sortOn, (\\))

type Board = [[Int]]

winningBoard :: [Board] -> [Int] -> (Board, [Int])
winningBoard boards draws = (wb, neededDraws)
  where
    wb = head $ sortOn (\b -> winningTurn b draws) boards
    turns = winningTurn wb draws
    neededDraws = take turns draws

loosingBoard :: [Board] -> [Int] -> (Board, [Int])
loosingBoard boards draws = (wb, neededDraws)
  where
    wb = last $ sortOn (\b -> winningTurn b draws) boards
    turns = winningTurn wb draws
    neededDraws = take turns draws

winningTurn :: Board -> [Int] -> Int
winningTurn board draws = rec draws []
  where
    rec [] drawed = length drawed + 1 --no win
    rec (h : t) drawed
      | hasBingo board drawed = length drawed
      | otherwise = rec t (h : drawed)

hasBingo :: Board -> [Int] -> Bool
hasBingo rows ds = any (\l -> isWinningLine l ds) lines
  where
    columns = transpose rows
    lines = rows ++ columns

isWinningLine :: [Int] -> [Int] -> Bool
isWinningLine line drawings = all (`elem` drawings) line

boardScore :: Board -> [Int] -> Int
boardScore board neededDraws = last neededDraws * sum boardLeftovers
  where
    boardLeftovers = concat board \\ neededDraws

bestScore :: [Board] -> [Int] -> Int
bestScore boards draws = boardScore wb nds
  where
    (wb, nds) = winningBoard boards draws

worstScore :: [Board] -> [Int] -> Int
worstScore boards draws = boardScore wb nds
  where
    (wb, nds) = loosingBoard boards draws

