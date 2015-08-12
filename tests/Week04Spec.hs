module Week04Spec where

import Test.Hspec
import Week04

spec :: Spec
spec = do
  describe "==" $
    it "should work" $ do
      P [1, 2, 3] `shouldBe` P [1, 2, 3]
      P [1, 2]    `shouldNotBe` P [1, 2, 3]

  describe "show" $
    it "should work" $ do
      show (P [1, 0, 0, 2]) `shouldBe` "2x^3 + 1"
      show (P [0, -1, 2])   `shouldBe` "2x^2 + -x"
      show (P [1, 2, 3])    `shouldBe` "3x^2 + 2x + 1"
      show (P [1, 0, 3])    `shouldBe` "3x^2 + 1"

  describe "plus" $
    it "should work" $ do
      P [5, 0, 1] + P [1, 1, 2] `shouldBe` P [6, 1, 3]
      P [1, 0, 1] + P [1, 1]    `shouldBe` P [2, 1, 1]

  describe "times" $
    it "should work" $
      P [1, 1, 1] * P [2, 2] `shouldBe` P [2, 4, 4, 2]

  describe "negate" $
    it "should work" $
      negate (P [1, 2, 3]) `shouldBe` P [-1, -2, -3]

  describe "applyP" $
    it "should work" $ do
      applyP (x^2 + 2*x + 1) 1 `shouldBe` 4
      applyP (x^2 + 2*x + 1) 2 `shouldBe` 9

  describe "deriv" $
    it "should work" $
      deriv (x^2 + 3*x + 5) `shouldBe` 2*x + 3
