module VirtualArrow.ElectionSpec (main, spec) where

import Test.Hspec
import VirtualArrow.Factory (input, oneDistrictProportionalityResult)
import VirtualArrow.Input
import VirtualArrow.Election

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "VirtualArrow.Election.oneDistrictProportionality" $
        it "returns the resulting parliament" $
            oneDistrictProportionality input `shouldBe` oneDistrictProportionalityResult
