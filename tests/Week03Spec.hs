module Week03Spec where

import Test.Hspec
import Week03

spec :: Spec
spec = do
  describe "factorial" $
    it "should work" $
      (run (extend empty "In" 10) factorial) "Out" `shouldBe` 3628800

  describe "squareRoot" $
    it "should work" $
      (run (extend empty "A" 100) squareRoot) "B" `shouldBe` 10

  describe "fibonacci" $
    it "should work" $
      (run (extend empty "In" 10) fibonacci) "Out" `shouldBe` 89
