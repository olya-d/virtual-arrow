{-# OPTIONS_GHC -fno-warn-orphans #-}

import qualified Interface.ReadCsv as Csv
import Interface.Interaction

import VirtualArrow.Input
import VirtualArrow.Election

import qualified Data.Map.Strict as Map


main :: IO ()
main = do
    ds <- Csv.readCSV "example/constituencies.csv" :: IO [District]
    vs <- Csv.readVotersFromCsv "example/voters.csv"
    let input = Input{ districts=ds
                     , voters=vs
                     , nparties=5
                     , districtMap=Map.fromList (votersByDistrict vs)}
    printParliament (plurality input)
