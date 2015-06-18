module Week01Spec where

import Test.Hspec
import Week01

spec :: Spec
spec = do
  describe "lastDigit" $
    it "should return the last digit" $ do
      lastDigit 123  `shouldBe` 3
      lastDigit 0    `shouldBe` 0
      lastDigit 123  `shouldBe` 3
      lastDigit 1234 `shouldBe` 4
      lastDigit 5    `shouldBe` 5
      lastDigit 10   `shouldBe` 0
      lastDigit 0    `shouldBe` 0

  describe "dropLastDigit" $
    it "should return the digits except last" $ do
      dropLastDigit 123  `shouldBe` 12
      dropLastDigit 5    `shouldBe` 0
      dropLastDigit 123  `shouldBe` 12
      dropLastDigit 1234 `shouldBe` 123
      dropLastDigit 5    `shouldBe` 0
      dropLastDigit 10   `shouldBe` 1
      dropLastDigit 0    `shouldBe` 0

  describe "toRevDigits" $
    it "should return the digits in reverse order" $ do
      toRevDigits 123   `shouldBe` [3, 2, 1]
      toRevDigits 5     `shouldBe` [5]
      toRevDigits 1234  `shouldBe` [4, 3, 2, 1]
      toRevDigits 10    `shouldBe` [0, 1]
      toRevDigits 0     `shouldBe` []
      toRevDigits (-17) `shouldBe` []

  describe "toDigits" $
    it "should return the digits in the right order" $ do
      toDigits 123   `shouldBe` [1, 2, 3]
      toDigits 5     `shouldBe` [5]
      toDigits 1234  `shouldBe` [1, 2, 3, 4]
      toDigits 10    `shouldBe` [1, 0]
      toDigits 0     `shouldBe` []
      toDigits (-17) `shouldBe` []

  describe "doubleEveryOther" $
    it "should double every other numbers, starting from the second number" $ do
      doubleEveryOther [4, 9, 5, 5] `shouldBe` [4, 18, 5, 10]
      doubleEveryOther [0, 0]       `shouldBe` [0, 0]

  describe "sumDigits" $
    it "should sum the digits in the provided list" $
      sumDigits [10, 5, 18, 4] `shouldBe` 1 + 0 + 5 + 1 + 8 + 4

  describe "luhn" $
    it "should return whether a credit card number is valid" $ do
      luhn 5594589764218858 `shouldBe` True
      luhn 1234567898765432 `shouldBe` False

  describe "hanoi" $
    it "should return a correct result for the puzzle" $
      hanoi 2 "a" "b" "c" `shouldBe` [("a","c"), ("a","b"), ("c","b")]
