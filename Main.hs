{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import System.Environment (getArgs)
import Control.Applicative()
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import Data.Csv
import qualified Data.Vector as V


{- Data types -}

-- List of parties ordered by voter's preference
type Preferences = [Int]
-- List of probabilities that voter will choose candidate from a party.
type Probabilities = [Float]

data District = District
    { district_identifier :: !Int
    , number_of_seats :: !Int
    }
    deriving (Show)

data Voter = Voter
    { voter_identifier :: !Int
    , district :: !Int
    , preferences :: Preferences
    , probabilities :: Probabilities
    }
    deriving (Show)

data Party = Party
    { party_identifier :: !Int
    }
    deriving (Show)

{--}


{- Definitions of records in CSV files corresponding to custom data types for Data.CSV -}

instance FromNamedRecord District where
    parseNamedRecord r = District <$> 
        r .: "district_identifier" <*> 
        r .: "number_of_seats"

instance FromNamedRecord Voter where
    parseNamedRecord r = Voter <$> 
        r .: "voter_identifier" <*>
        r .: "district" <*>
        r .: "preferences" <*>
        r .: "probabilities"

instance FromNamedRecord Party where
    parseNamedRecord r = Party <$> 
        r .: "party_identifier"

{--}


{- Definitons of parsing methods for custom data types for Data.CSV -}

splitOnColumns s = T.splitOn (T.pack ":") (T.pack s)

instance FromField Preferences where
    parseField s = 
        pure (map (read . T.unpack) (splitOnColumns c) :: Preferences)
        where
            c = BC.unpack s

instance FromField Probabilities where
    parseField s = 
        pure (map (read . T.unpack) (splitOnColumns c) :: Probabilities)
        where
            c = BC.unpack s
            
{--}

readCSV :: FromNamedRecord a => FilePath -> IO [a]
readCSV path = do
  c <- BL.readFile path
  case decodeByName c of
    Left err -> error err
    Right c' -> return $ V.toList $ snd c'


main :: IO ()
main = do
    args <- getArgs
    case args of
        [dfile, vfile, pfile] -> do
            districts <- readCSV dfile :: IO [District]
            voters <- readCSV vfile :: IO [Voter]
            parties <- readCSV pfile :: IO [Party]
            putStrLn (show districts)
            putStrLn (show voters)
            putStrLn (show parties)
        _ -> error "Wrong number of arguments."
