module VirtualArrow.UtilsSpec (main, spec) where

import Test.Hspec
import VirtualArrow.Utils


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "VirtaulArrow.Utils.frequencies" $
        it "returns list of frequencies of each item in the list" $
            frequencies [0 :: Int, 2, 0, 2, 4, 1, 0] `shouldBe` [(0, 3), (1, 1), (2, 2), (4, 1)]

    describe "VirtaulArrow.Utils.minIndex" $
        it "returns the index of minimum element" $ do
            minIndex [0 :: Int, 2, 0, 2, 4, 1, 0] `shouldBe` 0
            minIndex [1 :: Int, 4, 2, 0, 1] `shouldBe` 3