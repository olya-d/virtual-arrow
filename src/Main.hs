{-# OPTIONS_GHC -fno-warn-orphans #-}

import qualified Interface.ReadCsv as Csv
import qualified Interface.CommandLine as CL

import VirtualArrow.Input
import VirtualArrow.Election
import VirtualArrow.Indices

import Options.Applicative
import Data.Maybe (isNothing, fromMaybe)
import qualified Data.Map.Strict as Map
import Control.Monad


printParliament :: Parliament -> IO()
printParliament parliament = do
    putStrLn "party,number_of_seats"
    mapM_ (\(p, s)-> putStrLn $ show p ++ "," ++ show s) parliament

readInput :: String -> String -> Int -> IO Input
readInput dfile vfile nparties = do
    districts <- Csv.readCSV dfile :: IO [District]
    voters <- Csv.readVotersFromCsv vfile
    return Input{ districts=districts
                , voters=voters
                , nparties=nparties
                , districtMap=Map.fromList $ votersByDistrict voters}

runResult :: CL.ResultOptions -> IO()
runResult opts = do
    input <- readInput 
        (CL.districtCsv opts)
        (CL.votersCsv opts)
        (CL.numberOfParties opts)
    case CL.system opts of
        "borda" -> printParliament (bordaCount input)
        "one-district" -> printParliament (oneDistrictProportionality input)
        "plurality" -> printParliament (plurality input)
        "run-off" -> printParliament (runOffPlurality input)
        "multi-district" -> printParliament (multiDistrictProportionality input)
        "mm1" -> 
            if isNothing (CL.weight opts) 
                then error "Please specify weight." 
                else printParliament $
                    mixedMember1 input (fromMaybe 0.0 (CL.weight opts))
        "mm2" ->
            if isNothing (CL.share opts)
                then error "Please specify share."
                else printParliament $
                    mixedMember2 input (fromMaybe 0.0 (CL.share opts))
        "threshold" ->
            if isNothing (CL.threshold opts)
                then error "Please specify threshold." 
                else printParliament $
                    thresholdProportionality 
                        input 
                        (fromMaybe 0.0 (CL.threshold opts))
        "stv" -> do
            candidateList <- Csv.readCSV 
                (fromMaybe "" (CL.candidateListCsv opts)) :: IO [Candidate]
            printParliament $
                singleTransferableVote input (candidateMap candidateList)
        otherwise -> error "Invalid system."

runR :: CL.ROptions -> IO()
runR opts = do
    input <- readInput 
        (CL.rDistrictCsv opts) 
        (CL.rVotersCsv opts) 
        (CL.rNumberOfParties opts)
    parliament <- Csv.readParliamentFromCSV (CL.resultCSV opts) :: IO Parliament
    print $ representativeness input parliament
            

run :: CL.Command -> IO()
run cmd =
    case cmd of
        CL.Result opts -> runResult opts  
        CL.R opts -> runR opts 

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> CL.parser) fullDesc
