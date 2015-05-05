module VirtualArrow.InputSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import qualified VirtualArrow.Input as I


districts =
    [I.District{I.districtID=1, I.numberOfSeats=4}
    ,I.District{I.districtID=2, I.numberOfSeats=2}]

voters =
    [I.Voter{I.voterID=1, I.district=1, I.preferences=[0,1,2], I.probabilities=[0.5, 0.25, 0.25]}
    ,I.Voter{I.voterID=2, I.district=1, I.preferences=[1,2,0], I.probabilities=[0.5, 0.25, 0.25]}
    ,I.Voter{I.voterID=3, I.district=1, I.preferences=[0,2,1], I.probabilities=[0.5, 0.25, 0.25]}
    ,I.Voter{I.voterID=4, I.district=1, I.preferences=[0,1,2], I.probabilities=[0.5, 0.25, 0.25]}
    ,I.Voter{I.voterID=5, I.district=1, I.preferences=[1,0,2], I.probabilities=[0.5, 0.25, 0.25]}
    ,I.Voter{I.voterID=6, I.district=2, I.preferences=[1,0,2], I.probabilities=[0.5, 0.25, 0.25]}
    ,I.Voter{I.voterID=7, I.district=2, I.preferences=[1,0,2], I.probabilities=[0.5, 0.25, 0.25]}
    ,I.Voter{I.voterID=8, I.district=2, I.preferences=[1,2,0], I.probabilities=[0.5, 0.25, 0.25]}
    ]

parties =
    [I.Party{I.partyID=1}
    ,I.Party{I.partyID=2}
    ,I.Party{I.partyID=3}
    ]

input = I.Input{I.districts=districts, I.voters=voters}


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Input.getListOfNumberOfSeats" $ do

    it "returns the list of number of seats for every district" $ do
      (I.getListOfNumberOfSeats input) `shouldBe` [4, 2]
