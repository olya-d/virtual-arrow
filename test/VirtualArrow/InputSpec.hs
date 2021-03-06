module VirtualArrow.InputSpec (main, spec) where

import Test.Hspec
import VirtualArrow.Factory (input)
import VirtualArrow.Input
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "VirtualArrow.Input.prefToPlaces" $
        it "changes the list of order of preference to the list of places" $ do
            prefToPlaces (V.fromList [0,1,2]) `shouldBe` V.fromList [0, 1, 2]
            prefToPlaces (V.fromList [1,2,0]) `shouldBe` V.fromList [2, 0, 1]

    describe "VirtualArrow.Input.votersByDistrict" $
        it "returns list of voters in the each district" $
            map (\x -> (fst x, map district (snd x))) (Map.toList $ districtMap input) `shouldBe` [(1, [1,1,1,1,1]), (2, [2,2,2])]

    describe "VirtualArrow.Input.numberOfSeatsByDistrictID" $
        it "returns list of number of seats in the district" $ do
            numberOfSeatsByDistrictID input 1 `shouldBe` 4
            numberOfSeatsByDistrictID input 2 `shouldBe` 2

    describe "VirtualArrow.Input.numberOfSeatsByDistrict" $
        it "returns the total number of seats" $
            numberOfSeatsByDistrict input `shouldBe` [(1, 4), (2, 2)]

    describe "VirtualArrow.Input.firstChoices" $
        it "returns list of the first choice for each voter" $
            firstChoices input `shouldBe` [0, 1, 0, 0, 1, 1, 1, 1]

    describe "VirtualArrow.Input.firstChoicesAmongVoters" $
        it "returns list of the first choice for each voter" $ do
            firstChoicesAmongVoters (districtMap input Map.! 1) `shouldBe` [0, 1, 0, 0, 1]
            firstChoicesAmongVoters (districtMap input Map.! 2) `shouldBe` [1, 1, 1]

    describe "VirtualArrow.Input.numberOfSeats" $
        it "returns the total number of seats" $
            parliamentSize input `shouldBe` 6

    describe "VirtualArrow.Input.numberOfVoters" $
        it "returns the total number of voters" $
            nvoters input `shouldBe` 8

    describe "VirtualArrow.Input.calculateProportion" $
        it "returns the number of seats based on the number of votes" $ do
            calculateProportion input 3 `shouldBe` 2
            calculateProportion input 5 `shouldBe` 4
