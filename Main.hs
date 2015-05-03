{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import System.Environment (getArgs)
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import Data.Csv
import qualified Data.Vector as V

data District = District
    { district_identifier :: !String
    , number_of_seats :: !Int
    }

type Preferences = [String]
type Probabilities = [Float]

data Voter = Voter
    { voter_identifier :: !String
    , district :: !String
    , preferences :: Preferences
    , probabilities :: Probabilities
    }

data Party = Party
    { party_identifier :: !String
    }

instance FromNamedRecord District where
    parseNamedRecord r = District <$> r .: "district_identifier" <*> r .: "number_of_seats"

readDistricts :: FilePath -> IO()
readDistricts path = do
    csvData <- BL.readFile path
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, v) -> V.forM_ v $ \ p ->
            putStrLn $ district_identifier p ++ " contributes " ++ show (number_of_seats p) ++ " seats"

instance FromField Preferences where
    parseField s = 
        pure ((map T.unpack (T.splitOn (T.pack ":") (T.pack c))) :: Preferences)
        where
            c = BC.unpack s

instance FromField Probabilities where
    parseField s = 
        pure ((map (read . T.unpack) (T.splitOn (T.pack ":") (T.pack c))) :: Probabilities)
        where
            c = BC.unpack s

instance FromNamedRecord Voter where
    parseNamedRecord r = Voter <$> 
        r .: "voter_identifier" <*>
        r .: "district" <*>
        r .: "preferences" <*>
        r .: "probabilities"

instance FromNamedRecord Party where
    parseNamedRecord r = Party <$> 
        r .: "party_identifier"

readVoters :: FilePath -> IO()
readVoters path = do
    csvData <- BL.readFile path
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, v) -> V.forM_ v $ \ p ->
            putStrLn $ voter_identifier p ++ " in district " ++ (district p) ++ " has preferences " ++ show (preferences p) ++ " and probabilities " ++ show (probabilities p)

readParties :: FilePath -> IO()
readParties path = do
    csvData <- BL.readFile path
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, v) -> V.forM_ v $ \ p ->
            putStrLn $ "Party " ++ (party_identifier p)

main :: IO()
main = do
    args <- getArgs
    case args of 
        [dfile, vfile, pfile] -> do
            readDistricts dfile
            readVoters vfile
            readParties pfile
        _ -> putStrLn "Wrong number of arguments"
