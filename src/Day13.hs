module Day13
    ( origami1, fold, printPaper, origami2
    ) where

import Data.List
--import qualified Data.Map as Map
--import qualified Data.Char as Char
import Data.List.Split

--type Paper = [[Bool]] -- true if [y][x] has a dot
type Dots = [Position]
type Position = (Int, Int) --x, y 0,0 is top left
type Fold = String

printPaper :: Dots -> IO [()]
printPaper dots = mapM printLine [0..maxY]
  where
    maxX = maximum $ map fst dots
    maxY = maximum $ map snd dots
    printLine y = putStrLn $ line y
    line y = [charFor (x, y) | x <- [0..maxX]]
    charFor d = if (elem d dots) then '#' else '.'
  
origami1 :: [Position] -> [Fold] -> Int
origami1 dots (h : _) = length $ fold dots h

origami2 :: [Position] -> [Fold] -> [Position]
origami2 = foldl fold

fold :: [Position] -> Fold -> [Position]
fold dots foldString = nub $ inner (foldString !! 11)
  where
    [_, foldIndexS] = splitOn "=" foldString
    foldIndex = read foldIndexS :: Int
    inner 'x' = map foldDotHorizontal dots
    inner 'y' = map foldDotVertical dots
    foldDotHorizontal (x, y) = (foldAbs x, y)
    foldDotVertical (x, y) = (x, foldAbs y)
    foldAbs i = if i < foldIndex then i else foldIndex - abs (i - foldIndex)