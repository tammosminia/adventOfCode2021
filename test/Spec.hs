import Test.Hspec
import Test.QuickCheck
import Day1

main :: IO ()
main = hspec $ do
  describe "day1" $ do
    it "countIncreases" $ do
      countIncreases [] `shouldBe` 0
      countIncreases [199, 200, 208, 210, 200, 207, 240, 269, 260, 263] `shouldBe` 7

    it "slidingIncreases" $ do
      slidingIncreases [1, 2] `shouldBe` 0
      slidingIncreases [199, 200, 208, 210, 200, 207, 240, 269, 260, 263] `shouldBe` 5
