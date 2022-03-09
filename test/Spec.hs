import Test.Hspec
import Test.QuickCheck

import qualified Data.Set as Set
import qualified Data.Map as Map

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
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18

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

  describe "day11" $ do
    let example = ["5483143223","2745854711","5264556173","6141336146","6357385478","4167524645","2176841721","6882881134","4846848554","5283751526"]
--    it "step" $ do
--      let (g1, f1) = step (initGrid example)
--      putStrLn (show g1)
--      f1 `shouldBe` 0
--      let (g2, f2) = step g1
--      putStrLn (show g2)
--      f2 `shouldBe` 10

    it "dumboOctopus1" $ do
      dumboOctopus1 example 1 `shouldBe` 0
      dumboOctopus1 example 10 `shouldBe` 204
      dumboOctopus1 example 100 `shouldBe` 1656

    it "dumboOctopus2" $ do
      dumboOctopus2 example `shouldBe` 195


  describe "day12" $ do
    let example1 = ["start-A","start-b","A-c","A-b","b-d","A-end","b-end"]
    let exampleSL = ["dc-end","HN-start","start-kj","dc-start","dc-HN","LN-dc","HN-end","kj-sa","kj-HN","kj-dc"]
    let example2 = ["fs-end","he-DX","fs-he","start-DX","pj-DX","end-zg","zg-sl","zg-pj","pj-he","RW-he","fs-DX","pj-RW","zg-RW","start-pj","he-WI","zg-he","pj-fs","start-RW"]
    it "cavePaths1" $ do
      cavePaths1 example1 `shouldBe` 10
      cavePaths1 example2 `shouldBe` 226

    it "containsSmallOnce" $ do
      containsSmallOnce [] `shouldBe` True
      containsSmallOnce ["start", "a"] `shouldBe` True
      containsSmallOnce ["start", "a", "a"] `shouldBe` False
      containsSmallOnce ["a", "b"] `shouldBe` True
      containsSmallOnce ["a", "b", "a"] `shouldBe` False
      containsSmallOnce ["start", "HN", "dc", "HN", "kj", "sa", "kj"] `shouldBe` False

    it "canContinueTo" $ do
      canContinueTo ["start", "HN", "dc", "HN", "kj", "sa", "kj"] "dc" `shouldBe` False

    it "paths2" $ do
      elem ["start", "HN", "dc", "HN", "kj", "sa", "kj", "dc", "end"] (paths2 (initCave exampleSL)) `shouldBe` False

    it "cavePaths2" $ do
      cavePaths2 example1 `shouldBe` 36
      cavePaths2 exampleSL `shouldBe` 103
      cavePaths2 example2 `shouldBe` 3509

  describe "Day13" $ do
    let exampleDots = [(6,10),(0,14),(9,10),(0,3),(10,4),(4,11),(6,0),(6,12),(4,1),(0,13),(10,12),(3,4),(3,0),(8,4),(1,10),(2,14),(8,10),(9,0)]
    let exampleFolds = ["fold along y=7", "fold along x=5"]
    
    it "origami1" $ do
      origami1 exampleDots exampleFolds `shouldBe` 17

  describe "Day14" $ do
    let exampleRules = ["CH -> B","HH -> N","CB -> H","NH -> C","HB -> C","HC -> B","HN -> C","NN -> C","BH -> H","NC -> B","NB -> B","BN -> B","BB -> N","BC -> B","CC -> N","CN -> C"]
    let exampleInput = "NNCB"
    
    it "polymer1" $ do
      polymer1 exampleRules exampleInput 10 `shouldBe` 1588
      
    it "polymer3" $ do
      polymer3 exampleRules exampleInput 40 `shouldBe` 2188189693529

  describe "Day15" $ do
    let example = ["1163751742","1381373672","2136511328","3694931569","7463417111","1319128137","1359912421","3125421639","1293138521","2311944581"]
    
    it "chiton1" $ do
      chiton1 example `shouldBe` 40
      
    it "chiton2" $ do
      chiton2 example `shouldBe` 315

  describe "Day16" $ do
    it "bits1" $ do
      bits1 "D2FE28" `shouldBe` 6
      bits1 "8A004A801A8002F478" `shouldBe` 16
      bits1 "620080001611562C8802118E34" `shouldBe` 12
      bits1 "C0015000016115A2E0802F182340" `shouldBe` 23
      bits1 "A0016C880162017C3686B18A3D4780" `shouldBe` 31

    it "bits2" $ do
      bits2 "C200B40A82" `shouldBe` 3
      bits2 "04005AC33890" `shouldBe` 54
      bits2 "880086C3E88112" `shouldBe` 7
      bits2 "CE00C43D881120" `shouldBe` 9
      bits2 "D8005AC2A8F0" `shouldBe` 1
      bits2 "F600BC2D8F" `shouldBe` 0
      bits2 "9C005AC2F8F0" `shouldBe` 0
      bits2 "9C0141080250320F1802104A08" `shouldBe` 1

  describe "Day17" $ do
    it "trickShot1" $ do
      trickShot1 100 ((20, -5), (30, -10)) `shouldBe` 45

    it "hitsTarget" $ do
      hitsTarget (0, 0) ((0, -5), (0, -10)) `shouldBe` True
      hitsTarget (10, 0) ((10, 0), (10, 0)) `shouldBe` True
      hitsTarget (10, 1) ((20, 0), (30, 0)) `shouldBe` True
      hitsTarget (25, -7) ((20, -5), (30, -10)) `shouldBe` True
      hitsTarget (7, 2) ((20, -5), (30, -10)) `shouldBe` True
      hitsTarget (6, 3) ((20, -5), (30, -10)) `shouldBe` True
      hitsTarget (6, 9) ((20, -5), (30, -10)) `shouldBe` True
      hitsTarget (9, 0) ((20, -5), (30, -10)) `shouldBe` True
      hitsTarget (17, -4) ((20, -5), (30, -10)) `shouldBe` False

    it "trickShot2" $ do
      trickShot2 100 ((20, -5), (30, -10)) `shouldBe` 112

  describe "Day18" $ do
    it "paths and back" $ do
      let test s = show (pathsToNumber (numberToPaths (compileSnailNumber s))) `shouldBe` s
      test "[1,2]"
      test "[[1,2],3]"
      test "[[[[[9,8],1],2],3],4]"
      test "[7,[6,[5,[4,[3,2]]]]]"
      test "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"
      test "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
      test "[[[[4,3],4],4],[7,[[8,4],9]]]"

    it "add" $ do
      let test i1 i2 r = show (snailAdd (compileSnailNumber i1) (compileSnailNumber i2)) `shouldBe` r
      test "[1,2]" "[3,4]" "[[1,2],[3,4]]"
      test "[1,2]" "[[3,4],5]" "[[1,2],[[3,4],5]]"
      test "[[[[4,3],4],4],[7,[[8,4],9]]]" "[1,1]" "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"




