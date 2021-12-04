module Day3
    ( powerConsumption, stringToBinary, binaryToInt, oxygenRating, lifeSupportRating
    ) where

import Data.List (transpose)


type Bit = Integer --0 or 1
type Binary = [Bit]

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

mostCommon :: [Bit] -> Bit
mostCommon bits
  | zeroes > div (length bits) 2 = 0
  | otherwise = 1
  where 
    zeroes = count 0 bits
  
leastCommon :: [Bit] -> Bit
leastCommon x = flipBit (mostCommon x)
    
flipBit :: Bit -> Bit
flipBit 0 = 1
flipBit 1 = 0

binaryToInt :: Binary -> Integer
binaryToInt bs = binr 1 $ reverse bs
  where 
    binr _ [] = 0
    binr m (0 : t) = binr (m * 2) t
    binr m (1 : t) = m + binr (m * 2) t
      
stringToBinary :: String -> Binary
stringToBinary = map charToBit
      
charToBit :: Char -> Bit
charToBit '0' = 0
charToBit '1' = 1
      
gamma :: [Binary] -> Binary
gamma x = map mostCommon (transpose x)

epsilon :: [Binary] -> Binary
epsilon x = map leastCommon (transpose x)

powerConsumption :: [String] -> Integer
powerConsumption s = binaryToInt (gamma b) * binaryToInt (epsilon b)
  where b = map stringToBinary s


oxygenRating :: [Binary] -> Binary
oxygenRating bs = rating mostCommon bs 0

co2Rating :: [Binary] -> Binary
co2Rating bs = rating leastCommon bs 0

type Criterium = [Bit] -> Bit
rating :: Criterium -> [Binary] -> Int -> Binary
rating _ [b] _ = b
rating c bs p = rating c fbs (p + 1)
  where 
    column = map (!! p) bs
    cr = c column
    fbs = filter columnIs bs 
    columnIs bin = (bin !! p) == cr
    
lifeSupportRating :: [String] -> Integer
lifeSupportRating s = binaryToInt (oxygenRating b) * binaryToInt (co2Rating b)
  where b = map stringToBinary s

    