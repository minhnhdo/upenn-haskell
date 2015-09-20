module HW07Spec where

import Data.Vector as V
import Test.Hspec
import HW07

spec :: Spec
spec = do
  describe "liftM" $
    it "should work" $ do
      HW07.liftM (+1) (Just 5) `shouldBe` Just 6
      HW07.liftM (+1) Nothing  `shouldBe` Nothing

  describe "swapV" $
    it "should work" $ do
      swapV 0 2 (V.fromList [1,2,3]) `shouldBe` Just (V.fromList [3,2,1])
      swapV 0 2 (V.fromList [1,2])   `shouldBe` Nothing

  describe "mapM" $
    it "should work" $ do
      HW07.mapM Just [1..10]                 `shouldBe` Just [1..10]
      HW07.mapM id [Just 1, Just 2, Nothing] `shouldBe` Nothing

  describe "getElts" $
    it "should work" $ do
      getElts [1,3] (V.fromList [0..9])  `shouldBe` Just [1, 3]
      getElts [3,1] (V.fromList [0..9])  `shouldBe` Just [3, 1]
      getElts [1,10] (V.fromList [0..9]) `shouldBe` Nothing
      getElts [10,1] (V.fromList [0..9]) `shouldBe` Nothing

  describe "partitionAt" $
    it "should work" $ do
      partitionAt (V.fromList [5, 2, 8, 3, 6, 1]) 3 `shouldBe` (V.fromList [2, 1], 3, V.fromList [5, 8, 6])
      partitionAt (V.fromList [1, 6, 4, 7, 2, 4]) 2 `shouldBe` (V.fromList [1, 2], 4, V.fromList [6, 7, 4])
