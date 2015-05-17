{-|
Module: Interface.ReadCsv
Description: Utitlity functions for parsing csv files.

The module contains utitlity functions for parsing csv files.
The functions are defined using 
<http://hackage.haskell.org/package/cassava cassava> package.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables #-}

module Interface.ReadCsv
(
    readCSV,
    readVotersFromCsv,
    readParliamentFromCSV,
    readCoalitionsFromCSV
) where

import qualified VirtualArrow.Input as I
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import Data.Csv
import qualified Data.Csv.Streaming as Streaming
import Data.Maybe (fromMaybe)
import qualified Data.Foldable as F

data VoterCollection = VoterCollection
    { size :: Int
    , district :: Int
    , preferences :: I.Preferences
    }
    deriving (Show)

generateVoters :: [VoterCollection] -> [I.Voter]
generateVoters =
    concatMap (\c ->
        replicate (size c) I.Voter{ I.district=district c
                                  , I.preferences=preferences c})

instance FromNamedRecord I.District where
    parseNamedRecord r = I.District <$>
        r .: "districtID" <*>
        r .: "nseats"

instance FromNamedRecord VoterCollection where
    parseNamedRecord r = VoterCollection <$>
        r .: "size" <*>
        r .: "district" <*>
        r .: "preferences"

instance FromField I.Preferences where
    parseField s =
        pure $ V.fromList $
            map (fst . fromMaybe (0, "") . BC.readInt) (BC.split ':' s)

instance FromNamedRecord I.Candidate where
    parseNamedRecord r = I.Candidate <$> 
        r .: "candidateID" <*>
        r .: "party"

-- | Functions parses csv according to the expected format.
readCSV :: FromNamedRecord a => FilePath -> IO [a]
readCSV path = do
  c <- BL.readFile path
  case Streaming.decodeByName c of
    Left err -> error err
    Right (_, c') -> return $ F.toList c'

readVotersFromCsv :: FilePath -> IO [I.Voter]
readVotersFromCsv path = do
    collections <- readCSV path :: IO [VoterCollection]
    return (generateVoters collections)

readParliamentFromCSV :: FilePath -> IO [(I.Party, I.NumberOfSeats)]
readParliamentFromCSV path = do
    c <- BL.readFile path
    case decode HasHeader c of
        Left err -> error err
        Right v  -> return $ V.toList $
            V.map (\(party :: Int, seats :: Int) -> (party, seats)) v

readCoalitionsFromCSV :: FilePath -> IO [(I.Party, Int)]
readCoalitionsFromCSV path = do
    c <- BL.readFile path
    case decode HasHeader c of
        Left err -> error err
        Right v  -> return $ V.toList $
            V.map (\(party :: Int, coalition :: Int) -> (party, coalition)) v
