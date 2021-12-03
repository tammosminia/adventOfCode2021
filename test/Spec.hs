import Test.Hspec
import Test.QuickCheck
import Day1
import Day2

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

