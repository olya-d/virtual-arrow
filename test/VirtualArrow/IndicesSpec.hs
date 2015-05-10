module VirtualArrow.IndicesSpec (main, spec) where

import VirtualArrow.Factory
import VirtualArrow.Indices (representativeness)
import Test.Hspec


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "VirtualArrow.Indices.representativeness" $ do
        it "should be 0 in case of oneDistrictProportionality" $
            representativeness input oneDistrictProportionalityResult
            `shouldBe`
            0.0

        it "should return the index of representativeness" $ do
            representativeness input bordaCountResult `shouldBe` bordaCountR
            representativeness input mixedMember2Result `shouldBe` mixedMember2R