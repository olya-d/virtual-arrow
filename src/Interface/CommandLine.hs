{-|
Module: Interface.CommandLine
Description: Definitions of command-line commands and their parsers.

This module defines parsers for command-line commands using 
<http://hackage.haskell.org/package/optparse-applicative optparse-applicative>
package.
-}

module Interface.CommandLine
(
    Command(..),
    Options(..),
    -- GallagherOptions(..),
    -- GovernabilityOptions(..),
    parser
)
where

import Options.Applicative


-- data ResultOptions = ResultOptions
--     { system :: String
--     , districtCsv :: String
--     , votersCsv :: String
--     , numberOfParties :: Int
--     , candidateListCsv :: Maybe String 
--     , weight :: Maybe Double
--     , share :: Maybe Double
--     , threshold :: Maybe Double
--     }

-- data GallagherOptions = GallagherOptions
--     { rResultCSV :: String
--     , rDistrictCsv :: String
--     , rVotersCsv :: String
--     , rNumberOfParties :: Int
--     , rCandidateListCsv :: Maybe String 
--     }

-- data GovernabilityOptions = GovernabilityOptions
--     { gResultCSV :: String
--     , gCoalitionCsv :: String 
--     }

data Options = ResultOptions { system :: String
    , districtCsv :: String
    , votersCsv :: String
    , numberOfParties :: Int
    , candidateListCsv :: Maybe String 
    , weight :: Maybe Double
    , share :: Maybe Double
    , threshold :: Maybe Double
    } | GallagherOptions
    { rResultCSV :: String
    , rDistrictCsv :: String
    , rVotersCsv :: String
    , rNumberOfParties :: Int
    , rCandidateListCsv :: Maybe String 
    } | GovernabilityOptions
    { gResultCSV :: String
    , gCoalitionCsv :: String 
    }

data Command = 
      Result Options 
    | Gallagher Options 
    | Governability Options

parseResultOptions :: Parser Options
parseResultOptions = ResultOptions
    <$> strOption
        ( long "system"
        <> short 's'
        <> metavar "ELECTORAL SYSTEM"
        <> help "borda | one-district | plurality | run-off | multi-district | mm1 | mm2 | threshold | stv" )
    <*> strOption
        ( long "districts_csv"
        <> short 'd'
        <> help "File should contain header districtID,nseats" )
    <*> strOption
        ( long "voters_csv"
        <> short 'v'
        <> help "File should contain header voterID,district,preferences")
    <*> option auto
        ( long "number_of_parties"
        <> short 'p'
        <> help "Number of parties")
    <*> optional (strOption
        ( long "candidate_list_csv"
        <> short 'c'
        <> help "Required in case of stv. File should contain header candidateID,party"))
    <*> optional (option auto
        ( long "weight"
        <> short 'w'
        <> help "Required in case of mm1."))
    <*> optional (option auto
        ( long "share"
        <> help "Required in case of mm2."))
    <*> optional (option auto
        ( long "threshold"
        <> short 't'
        <> help "Required in case of threshold."))


parseGallagherOptions :: Parser Options
parseGallagherOptions = GallagherOptions
    <$> strOption
        ( long "result_csv"
        <> short 'r'
        <> help "Path to the output of the result command.")
    <*> strOption
        ( long "districts_csv"
        <> short 'd'
        <> help "File should contain header districtID,nseats" )
    <*> strOption
        ( long "voters_csv"
        <> short 'v'
        <> help "File should contain header voterID,district,preferences")
    <*> option auto
        ( long "number_of_parties"
        <> short 'p'
        <> help "Number of parties")
    <*> optional (strOption
        ( long "candidate_list_csv"
        <> short 'c'
        <> help "Required in case of stv. File should contain header candidateID,party"))

parseGovernabilityOptions :: Parser Options
parseGovernabilityOptions = GovernabilityOptions
    <$> strOption
        ( long "result_csv"
        <> short 'r'
        <> help "Path to the output of the result command.")
    <*> strOption
        ( long "coalition_csv"
        <> short 'c'
        <> help "File should contain header partyID,coalitionID")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

-- | Parses the call of virtual-arrow from the command-line.
parser :: Parser Command
parser = subparser (
    (command "result" ((Result <$> parseResultOptions) `withInfo` "Output the resulting parliament." ))
 <> (command "gallagher" ((Gallagher <$> parseGallagherOptions) `withInfo` "Calculate the index of representativeness according to Gallagher." ))
 <> (command "governability" ((Governability <$> parseGovernabilityOptions) `withInfo` "Calculate the index of governability." )) )
