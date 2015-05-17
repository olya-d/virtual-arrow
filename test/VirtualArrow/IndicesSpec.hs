module VirtualArrow.IndicesSpec (main, spec) where

import VirtualArrow.Factory
import VirtualArrow.Indices (gallagherIndex)
import Test.Hspec


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "VirtualArrow.Indices.gallagherIndex" $ do
        it "should be 0 in case of oneDistrictProportionality" $
            gallagherIndex input oneDistrictProportionalityResult Nothing
            `shouldBe`
            0.0

        it "should return the index of gallagherIndex" $ do
            gallagherIndex input bordaCountResult Nothing `shouldBe` bordaCountR
            gallagherIndex input mixedMember2Result Nothing `shouldBe` mixedMember2R