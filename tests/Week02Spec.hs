module Week02Spec where

import Test.Hspec
import Week02

spec :: Spec
spec = do
  describe "exactMatches" $
    it "should return the number of exact matches between the actual code and the guess" $ do
      exactMatches [Red, Blue, Green, Yellow] [Blue, Green, Yellow, Red]   `shouldBe` 0
      exactMatches [Red, Blue, Green, Yellow] [Red, Purple, Green, Orange] `shouldBe` 2

  describe "countColors" $ do
    it "should count the number of times a peg appears in a list" $ do
      countColors [Red, Blue, Yellow, Purple] `shouldBe` [1, 0, 1, 1, 0, 1]
      countColors [Green, Blue, Green, Orange] `shouldBe` [0, 2, 1, 0, 1, 0]
    it "should return a list of 6 items regardless of the length of the input code" $
      length (countColors [Red]) `shouldBe` 6
    it "should return a list whose sum is equal to the length of the input code" $
      sum (countColors [Red, Blue, Yellow, Purple]) `shouldBe` length [Red, Blue, Yellow, Purple]

  describe "matches" $
    it "should count the number of matches between the actual code and the guess" $
      matches [Red, Blue, Yellow, Orange] [Red, Orange, Orange, Blue] `shouldBe` 3

  describe "getMove" $
    it "should construct a move from a guess given the actual code" $
      getMove [Red, Blue, Yellow, Orange] [Red, Orange, Orange, Blue] `shouldBe` Move [Red, Orange, Orange, Blue] 1 2

  describe "isConsistent" $
    it "should work" $ do
      isConsistent (Move [Red, Red, Blue, Green] 1 1) [Red, Blue, Yellow, Purple] `shouldBe` True
      isConsistent (Move [Red, Red, Blue, Green] 1 1) [Red, Blue, Red, Purple] `shouldBe` False
