{-# OPTIONS_GHC -fno-warn-orphans #-}

import qualified Interface.ReadCsv as Csv
import qualified Interface.CommandLine as CL

import VirtualArrow.Input
import VirtualArrow.Election


import Options.Applicative
import Data.Maybe (isNothing, fromMaybe)
import Control.Arrow ((&&&))
import qualified Data.Map.Strict as Map
import Control.Monad

import Data.List (groupBy)
import Data.Function (on)

printParliament :: Parliament -> IO()
printParliament parliament = do
    putStrLn "party,number_of_seats"
    mapM_ (\(p, s)-> putStrLn $ (show p) ++ "," ++ (show s)) parliament

main :: IO ()
main = do
    districts <- Csv.readCSV "example/constituencies.csv" :: IO [District]
    voters <- Csv.readVotersFromCsv "example/voters.csv"
    let input = Input{ districts=districts
                     , voters=voters
                     , nparties=5
                     , districtMap=Map.fromList (votersByDistrict voters)}
    printParliament (plurality input)
