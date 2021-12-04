import Test.Hspec
import Test.QuickCheck
import Day1
import Day2
import Day3

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


