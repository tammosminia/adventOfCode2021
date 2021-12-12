import Test.Hspec
import Test.QuickCheck

import qualified Data.Set as Set

import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Day10

main :: IO ()
main = hspec $ do
  describe "day1" $ do
    let example = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
    it "countIncreases" $ do
      countIncreases [] `shouldBe` 0
      countIncreases example `shouldBe` 7

    it "slidingIncreases" $ do
      slidingIncreases [1, 2] `shouldBe` 0
      slidingIncreases example `shouldBe` 5

  describe "day2" $ do
    let example = [forward 5, down 5, forward 8, up 3, down 8, forward 2]
    it "headingFor" $ do
      headingFor [] `shouldBe` (0, 0)
      headingFor example `shouldBe` (15, 10)

    it "aimFor" $ do
      aimFor [] `shouldBe` (0, 0)
      aimFor [down 2, forward 5] `shouldBe` (5, 10)
      aimFor example `shouldBe` (15, 60)

  describe "day3" $ do
    let example = ["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"]
    it "powerConsumption" $ do
      powerConsumption example `shouldBe` 198

    it "stringToBinary" $ do
      stringToBinary "101" `shouldBe` [1,0,1]

    it "binaryToInt" $ do
      binaryToInt [1, 0, 1] `shouldBe` 5
      binaryToInt [0, 1, 0] `shouldBe` 2

    it "oxygenRating" $ do
      oxygenRating (map stringToBinary example) `shouldBe` stringToBinary "10111"

    it "lifeSupportRating" $ do
      lifeSupportRating example `shouldBe` 230

  describe "day4" $ do
    let exampleDrawings = [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]
    let exampleBoards = [ [[22,13,17,11,0],[8,2,23,4,24],[21,9,14,16,7],[6,10,3,18,5],[1,12,20,15,19]], [[3,15,0,2,22],[9,18,13,17,5],[19,8,7,25,23],[20,11,10,24,4],[14,21,16,12,6]], [[14,21,17,24,4],[10,16,15,9,19],[18,8,23,26,20],[22,11,13,6,5],[2,0,12,3,7]] ]
    it "bestScore" $ do
      bestScore exampleBoards exampleDrawings `shouldBe` 4512

    it "worstScore" $ do
      worstScore exampleBoards exampleDrawings `shouldBe` 1924

  describe "day5" $ do
    let example = [((0,9), (5,9)), ((8,0), (0,8)), ((9,4), (3,4)), ((2,2), (2,1)), ((7,0), (7,4)), ((6,4), (2,0)), ((0,9), (2,9)), ((3,4), (1,4)), ((0,0), (8,8)), ((5,5), (8,2))]
    it "dangerCount1" $ do
      dangerCount1 example `shouldBe` 5

    it "dangerCount2" $ do
      dangerCount2 example `shouldBe` 12

    it "crosses2" $ do
      crosses2 ((0,0),(1,1)) (0,0) `shouldBe` True
      crosses2 ((0,0),(1,1)) (1,0) `shouldBe` False
      crosses2 ((0,0),(1,1)) (2,2) `shouldBe` False
      crosses2 ((0,0),(0,100)) (0,2) `shouldBe` True
      crosses2 ((0,0),(0,100)) (1,2) `shouldBe` False
      crosses2 ((0,0),(100,100)) (1,2) `shouldBe` False
      crosses2 ((0,0),(100,100)) (1,1) `shouldBe` True
      crosses2 ((0,0),(100,100)) (10,10) `shouldBe` True
      crosses2 ((0,0),(100,100)) (10,20) `shouldBe` False
      crosses2 ((8,0),(0,8)) (8,0) `shouldBe` True
      crosses2 ((8,0),(0,8)) (0,8) `shouldBe` True
      crosses2 ((8,0),(0,8)) (4,4) `shouldBe` True
      crosses2 ((8,0),(0,8)) (3,5) `shouldBe` True
      crosses2 ((8,0),(0,8)) (3,3) `shouldBe` False

  describe "day6" $ do
    let example = [3,4,3,1,2]
    it "lanternfish" $ do
      lanternfish 18 example `shouldBe` 26
      lanternfish 80 example `shouldBe` 5934
      lanternfish 256 example `shouldBe` 26984457539

  describe "day7" $ do
    let example = [16,1,2,0,4,2,7,1,2,14]
    it "crabs" $ do
      crabs example `shouldBe` 37

    it "crabs2" $ do
      crabs2 example `shouldBe` 168

  describe "day8" $ do
    let example = ["egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb", "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"]
    it "digits1" $ do
      digits1 example `shouldBe` 6

    it "digits2" $ do
      digits2 example `shouldBe` 8717 + 4315

    it "decrypt" $ do
      length (Set.fromList (decrypt (fst (parseInputLine (head example))))) `shouldBe` 10
      
    it "decode" $ do
      let encoding = decrypt (fst (parseInputLine (head example)))
      decode encoding [Set.fromList "fecab"] `shouldBe` 2
      
    it "parseInputLine" $ do
      let p = parseInputLine (head example)
      length (fst p) `shouldBe` 10
      length (snd p) `shouldBe` 4

  describe "day9" $ do
    let example = ["2199943210","3987894921","9856789892","8767896789","9899965678"]
    it "lavaTube1" $ do
      lavaTube1 example `shouldBe` 15

    it "lavaTube2" $ do
      lavaTube2 example `shouldBe` 1134

  describe "day10" $ do
    let example = ["[({(<(())[]>[[{[]{<()<>>","[(()[<>])]({[<{<<[]>>(","{([(<{}[<>[]}>{[]{[(<()>","(((({<>}<{<{<>}{[]{[]{}","[[<[([]))<([[{}[[()]]]","[{[{({}]{}}([{[{{{}}([]","{<[[]]>}<{[{[{[]{()[[[]","[<(<(<(<{}))><([]([]()","<{([([[(<>()){}]>(<<{{","<{([{{}}[<[[[<>{}]]]>[]]"]
    it "parseLines1" $ do
      parseLines1 ["(]"] `shouldBe` 57
      parseLines1 ["{()()()>"] `shouldBe` 25137
      parseLines1 ["(((()))}"] `shouldBe` 1197
      parseLines1 ["(]", "()", "(((()))}"] `shouldBe` 57 + 1197
      parseLines1 ["{([(<{}[<>[]}>{[]{[(<()>"] `shouldBe` 1197
      parseLines1 example `shouldBe` 26397

    it "parseLines2" $ do
      parseLines2 example `shouldBe` 288957


