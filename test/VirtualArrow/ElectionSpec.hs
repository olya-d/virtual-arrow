module VirtualArrow.ElectionSpec (main, spec) where

import Test.Hspec
import VirtualArrow.Factory
import VirtualArrow.Election
import qualified Data.Map as M

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

    describe "VirtualArrow.Election.mixedMember1 with 0" $
        it "returns the resulting parliament" $
            mixedMember1 input 0 `shouldBe` multiDistrictProportionalityResult

    describe "VirtualArrow.Election.mixedMember1 with 1" $
        it "returns the resulting parliament" $
            mixedMember1 input 1 `shouldBe` pluralityResult

    describe "VirtualArrow.Election.mixedMember1 with 0.5" $
        it "returns the resulting parliament" $
            mixedMember1 input 0.5 `shouldBe` mixedMember1Result05

    describe "VirtualArrow.Election.mixedMember2" $
        it "returns the resulting parliament" $
            mixedMember2 input 0.5 `shouldBe` mixedMember2Result

    describe "VirtualArrow.Election.thresholdProportionality" $
        it "returns the resulting parliament" $ do
            thresholdProportionality input 0.5 `shouldBe` thresholdProportionalityResult05
            thresholdProportionality input 0.2 `shouldBe` thresholdProportionalityResult02

    describe "VirtualArrow.Election.singleTransferableVote" $
        it "returns the resulting parliament" $
            singleTransferableVote input2 (M.fromList [(0, 0), (1, 0), (2, 1)]) `shouldBe` singleTransferableVoteResult
