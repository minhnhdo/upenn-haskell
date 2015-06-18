module Week02Spec where

import Test.Hspec
import Week02

spec :: Spec
spec = do
  describe "exactMatches" $
    it "should return the number of exact matches between the actual code and the guess" $ do
      exactMatches [Red, Blue, Green, Yellow] [Blue, Green, Yellow, Red]   `shouldBe` 0
      exactMatches [Red, Blue, Green, Yellow] [Red, Purple, Green, Orange] `shouldBe` 2
