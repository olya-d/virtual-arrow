{-# OPTIONS_GHC -fno-warn-orphans #-}

import qualified Interface.ReadCsv as Csv
import qualified Interface.CommandLine as CL

import VirtualArrow.Input
import VirtualArrow.Election


import Options.Applicative
import Data.Maybe (isNothing, fromMaybe)
import Control.Arrow ((&&&))
import qualified Data.Map.Strict as Map


candidateMap :: [Candidate] -> Map.Map Int Int
candidateMap list = Map.fromList $ 
    map (candidateID Control.Arrow.&&& party) list

run :: CL.VirtualArrow -> IO()
run (CL.VirtualArrow system dfile vfile nparties cfile w s t) = do
    districts <- Csv.readCSV dfile :: IO [District]
    voters <- Csv.readCSV vfile :: IO [Voter]
    let input = Input{ districts=districts
                     , voters=voters
                     , nparties=nparties}
    case system of
        "borda" -> print (bordaCount input)
        "one-district" -> print (oneDistrictProportionality input)
        "plurality" -> print (plurality input)
        "run-off" -> print (runOffPlurality input)
        "multi-district" -> print (multiDistrictProportionality input)
        "mm1" -> 
            if isNothing w then error "Please specify weight." else
                print (mixedMember1 input (fromMaybe 0.0 w))
        "mm2" ->
            if isNothing s then error "Please specify share." else
                print (mixedMember2 input (fromMaybe 0.0 s))
        "threshold" ->
            if isNothing t then error "Please specify threshold." else
                print (thresholdProportionality input (fromMaybe 0.0 t))
        "stv" -> do
            candidateList <- Csv.readCSV (fromMaybe "" cfile) :: IO [Candidate]
            print (singleTransferableVote input (candidateMap candidateList))
        otherwise -> error "Invalid system."


main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> CL.virtualarrow) fullDesc
