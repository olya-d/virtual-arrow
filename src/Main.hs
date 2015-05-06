{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import System.Environment (getArgs)
import Control.Applicative()
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import Data.Csv
import qualified Data.Vector as V

import qualified VirtualArrow.Input as I
import VirtualArrow.Election (oneDistrictProportionality)

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


main :: IO ()
main = do
    args <- getArgs
    case args of
        [dfile, vfile] -> do
            districts <- readCSV dfile :: IO [I.District]
            voters <- readCSV vfile :: IO [I.Voter]
            print (oneDistrictProportionality I.Input{I.districts=districts, I.voters=voters, I.numOfParties=3})
        _ -> error "Wrong number of arguments."
