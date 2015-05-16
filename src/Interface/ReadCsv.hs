{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables #-}

module Interface.ReadCsv
(
    readCSV,
    readParliamentFromCSV
) where

import qualified VirtualArrow.Input as I
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import Data.Csv
import Data.Maybe (fromMaybe)


instance FromNamedRecord I.District where
    parseNamedRecord r = I.District <$> 
        r .: "districtID" <*> 
        r .: "nseats"

instance FromNamedRecord I.Voter where
    parseNamedRecord r = I.Voter <$> 
        r .: "voterID" <*>
        r .: "district" <*>
        r .: "preferences"

instance FromNamedRecord I.Candidate where
    parseNamedRecord r = I.Candidate <$> 
        r .: "candidateID" <*>
        r .: "party"

instance FromField I.Preferences where
    parseField s = 
        pure $ V.fromList $
            map (fst . fromMaybe (0, "") . BC.readInt) (BC.split ':' s)


readCSV :: FromNamedRecord a => FilePath -> IO [a]
readCSV path = do
  c <- BL.readFile path
  case decodeByName c of
    Left err -> error err
    Right c' -> return $ V.toList $ snd c'

readParliamentFromCSV :: FilePath -> IO [(Int, Int)]
readParliamentFromCSV path = do
    c <- BL.readFile path
    case decode HasHeader c of
        Left err -> error err
        Right v  -> return $ V.toList $ V.map (\(party :: Int, seats :: Int) ->
                    (party, seats)) v
