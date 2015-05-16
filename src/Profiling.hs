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


candidateMap :: [Candidate] -> Map.Map Int Int
candidateMap list = Map.fromList $ 
    map (candidateID Control.Arrow.&&& party) list


printParliament :: Parliament -> IO()
printParliament parliament = do
    putStrLn "party,number_of_seats"
    mapM_ (\(p, s)-> putStrLn $ (show p) ++ "," ++ (show s)) parliament

main :: IO ()
main = do
    districts <- Csv.readDistricts "uk/constituencies.csv" :: IO [District]
    voters <- Csv.readCSV "uk/voters.csv" :: IO [Voter]
    let input = Input{ districts=districts
                     , voters=voters
                     , nparties=5}
    printParliament (plurality input)