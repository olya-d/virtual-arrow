{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import System.Environment (getArgs)
import Control.Applicative()
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import Data.Csv
import qualified Data.Vector as V
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import Data.Function (on)

import qualified VirtualArrow.Input as I
import VirtualArrow.Election (bordaCount, oneDistrictProportionality)

{--}


{- Definitions of records in CSV files corresponding to custom data types for Data.CSV -}

instance FromNamedRecord I.District where
    parseNamedRecord r = I.District <$> 
        r .: "districtID" <*> 
        r .: "seats"

instance FromNamedRecord I.Voter where
    parseNamedRecord r = I.Voter <$> 
        r .: "voterID" <*>
        r .: "district" <*>
        r .: "preferences" <*>
        r .: "probabilities"

instance FromNamedRecord I.Party where
    parseNamedRecord r = I.Party <$> 
        r .: "partyID"

{--}


{- Definitons of parsing methods for custom data types for Data.CSV -}
splitOnColumns :: String -> [T.Text]
splitOnColumns s = T.splitOn (T.pack ":") (T.pack s)

instance FromField I.Preferences where
    parseField s = 
        pure (map (read . T.unpack) (splitOnColumns c) :: I.Preferences)
        where
            c = BC.unpack s

instance FromField I.Probabilities where
    parseField s = 
        pure (map (read . T.unpack) (splitOnColumns c) :: I.Probabilities)
        where
            c = BC.unpack s
            
{--}

readCSV :: FromNamedRecord a => FilePath -> IO [a]
readCSV path = do
  c <- BL.readFile path
  case decodeByName c of
    Left err -> error err
    Right c' -> return $ V.toList $ snd c'


convertVoterToTuple :: I.Voter -> ([Int], [Float])
convertVoterToTuple v = (I.preferences v, I.probabilities v)

sortDistrictsById :: [I.District] -> [I.District]
sortDistrictsById = sortBy (comparing I.districtID)

convertToList :: [I.District] -> [I.Voter] -> [([([Int], [Float])], Int)]
convertToList districts voters =
    [(grouped !! i, I.seats (sorted !! i)) | i <- [0..length grouped - 1]]
    where
        grouped = [map convertVoterToTuple listOfVoters | listOfVoters <- groupBy ((==) `on` I.district) voters ]
        sorted = sortDistrictsById districts

main :: IO ()
main = do
    args <- getArgs
    case args of
        [dfile, vfile, pfile] -> do
            districts <- readCSV dfile :: IO [I.District]
            voters <- readCSV vfile :: IO [I.Voter]
            print (oneDistrictProportionality I.Input{I.districts=districts, I.voters=voters, I.numOfParties=3})
        _ -> error "Wrong number of arguments."
