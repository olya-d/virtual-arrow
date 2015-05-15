module Interface.CommandLine
(
    Command(..),
    ResultOptions(..),
    ROptions(..),
    parser
)
where

import Options.Applicative


data ResultOptions = ResultOptions
    { system :: String
    , districtCsv :: String
    , votersCsv :: String
    , numberOfParties :: Int
    , candidateListCsv :: Maybe String 
    , weight :: Maybe Double
    , share :: Maybe Double
    , threshold :: Maybe Double
    }

data ROptions = ROptions
    { resultCSV :: String
    }

data Command = Result ResultOptions | R ROptions

parseResultOptions :: Parser ResultOptions
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
        <> short 'p')
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


parseROptions :: Parser ROptions
parseROptions = ROptions
    <$> strOption
        ( long "result_csv"
        <> short 'r'
        <> help "Path to the output of the result command."
        )

parser :: Parser Command
parser =
    subparser
      ( command "result" (info (Result <$> parseResultOptions )
        ( progDesc "Output the resulting parliament" ))
      <> command "r" (info (R <$> parseROptions )
        ( progDesc "Calculate the index of representativeness" ))
      )
