module VirtualArrow.ElectionSpec (main, spec) where

import Test.Hspec
import VirtualArrow.Factory
import VirtualArrow.Input
import VirtualArrow.Election

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "VirtualArrow.Election.oneDistrictProportionality" $
        it "returns the resulting parliament" $
            oneDistrictProportionality input `shouldBe` oneDistrictProportionalityResult

    describe "VirtualArrow.Election.bordaCount" $
        it "returns the resulting parliament" $
            bordaCount input `shouldBe` bordaCountResult

    describe "VirtualArrow.Election.plurality" $
        it "returns the resulting parliament" $
            plurality input `shouldBe` pluralityResult