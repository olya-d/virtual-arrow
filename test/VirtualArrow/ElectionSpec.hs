module VirtualArrow.ElectionSpec (main, spec) where

import Test.Hspec
import VirtualArrow.Factory
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

    describe "VirtualArrow.Election.runOffPlurality" $
        it "returns the resulting parliament" $
            runOffPlurality input `shouldBe` runOffPluralityResult

    describe "VirtualArrow.Election.multiDistrictProportionality" $
        it "returns the resulting parliament" $
            multiDistrictProportionality input `shouldBe` multiDistrictProportionalityResult

    describe "VirtualArrow.Election.mixedMember1Result" $
        it "returns the resulting parliament" $
            mixedMember1 input 0.5 `shouldBe` mixedMember1Result
