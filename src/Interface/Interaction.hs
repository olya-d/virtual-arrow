{-|
Module: Interface.Interaction
Description: Utitlities for interacting with the user.

This module contains functions that process input from command-line
and output the result. It is used by "Main".
-}
module Interface.Interaction
(
    printParliament,
    readInput,
    run
)
where

import qualified Interface.ReadCsv as Csv
import qualified Interface.CommandLine as CL

import VirtualArrow.Input
import VirtualArrow.Election
import VirtualArrow.Indices

import Data.Maybe (isNothing, fromMaybe)
import qualified Data.Map.Strict as Map


-- | Outputs the parliament in a csv format with columns "party,nubmer_of_seats".
printParliament :: Parliament -> IO()
printParliament parliament = do
    putStrLn "party,number_of_seats"
    mapM_ (\(p, s)-> putStrLn $ show p ++ "," ++ show s) parliament

-- | Reads "VirtualArrow.Input.Input" data type from supplied csv files.
-- Used by 'runResultCommand' and 'runRCommand'.
readInput :: String  -- ^ file with districts
          -> String -- ^ file with voters
          -> Int -- ^ number of parties
          -> IO Input
readInput dfile vfile np = do
    ds <- Csv.readCSV dfile :: IO [District]
    vs <- Csv.readVotersFromCsv vfile
    return Input{ districts=ds
                , voters=vs
                , nparties=np
                , districtMap=Map.fromList $ votersByDistrict vs}

-- | Parses command-line arguments passed to @result@ command and outputs the
-- resulting parliament. Used by 'run'.
runResultCommand :: CL.ResultOptions -> IO()
runResultCommand opts = do
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
        _ -> error "Invalid system."

-- | Parses command-line arguments passed to @gallagher@ command and outputs the
-- index of representativeness. Used by 'run'.
runGallagherCommand :: CL.GallagherOptions -> IO()
runGallagherCommand opts = do
    input <- readInput 
        (CL.rDistrictCsv opts) 
        (CL.rVotersCsv opts) 
        (CL.rNumberOfParties opts)
    parliament <- Csv.readParliamentFromCSV (CL.rResultCSV opts) :: IO Parliament
    case CL.rCandidateListCsv opts of
        Just ccsv -> do
            candidates <- Csv.readCSV ccsv :: IO [Candidate]
            print $ gallagherIndex input parliament (Just (candidateMap candidates))
        Nothing ->
            print $ gallagherIndex input parliament Nothing

-- | Parses command-line arguments passed to @governability@ command and outputs 
-- the index of governability. Used by 'run'.
runGovernabilityCommand :: CL.GovernabilityOptions -> IO()
runGovernabilityCommand opts = do
    coalitions <- Csv.readCoalitionsFromCSV (CL.gCoalitionCsv opts)
    parliament <- Csv.readParliamentFromCSV (CL.gResultCSV opts)
    print $ governability parliament coalitions

-- | Read command-line arguments and starts processing of the supplied command.
-- Used by "Main" in the main function.
run :: CL.Command -> IO()
run cmd =
    case cmd of
        CL.Result opts -> runResultCommand opts  
        CL.Gallagher opts -> runGallagherCommand opts 
        CL.Governability opts -> runGovernabilityCommand opts