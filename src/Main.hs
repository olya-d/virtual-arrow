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


run :: CL.VirtualArrow -> IO()
run (CL.VirtualArrow system dfile vfile nparties cfile w s t) = do
    districts <- Csv.readCSV dfile :: IO [District]
    voters <- Csv.readCSV vfile :: IO [Voter]
    let input = Input{ districts=districts
                     , voters=voters
                     , nparties=nparties}
    case system of
        "borda" -> printParliament (bordaCount input)
        "one-district" -> printParliament (oneDistrictProportionality input)
        "plurality" -> printParliament (plurality input)
        "run-off" -> printParliament (runOffPlurality input)
        "multi-district" -> printParliament (multiDistrictProportionality input)
        "mm1" -> 
            if isNothing w then error "Please specify weight." else
                printParliament (mixedMember1 input (fromMaybe 0.0 w))
        "mm2" ->
            if isNothing s then error "Please specify share." else
                printParliament (mixedMember2 input (fromMaybe 0.0 s))
        "threshold" ->
            if isNothing t then error "Please specify threshold." else
                printParliament (thresholdProportionality input (fromMaybe 0.0 t))
        "stv" -> do
            candidateList <- Csv.readCSV (fromMaybe "" cfile) :: IO [Candidate]
            printParliament (singleTransferableVote input (candidateMap candidateList))
        otherwise -> error "Invalid system."


main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> CL.virtualarrow) fullDesc
