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


candidateMap :: [Candidate] -> Map.Map Int Int
candidateMap list = Map.fromList $
    map (candidateID Control.Arrow.&&& party) list


printParliament :: Parliament -> IO()
printParliament parliament = do
    putStrLn "party,number_of_seats"
    mapM_ (\(p, s)-> putStrLn $ (show p) ++ "," ++ (show s)) parliament


votersByDistrictID :: [Voter] -> DistrictID -> [Voter]
votersByDistrictID voters dId = filter ((== dId) . district) voters

votersByDistrict :: [Voter] -> [(DistrictID, [Voter])]
votersByDistrict voters =
    map (\g -> (district (head g), g)) (groupBy ((==) `on` district) voters)


main :: IO ()
main = do
    districts <- Csv.readCSV "uk/constituencies.csv" :: IO [District]
    voters <- Csv.readVotersCsv "uk/voters.csv"
    let input = Input{ districts=districts
                     , voters=voters
                     , nparties=5
                     , districtMap=Map.fromList (Main.votersByDistrict voters)}
    printParliament (plurality input)