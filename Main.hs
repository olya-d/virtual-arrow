{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V

data District = District
    { identifier   :: !String
    , number_of_seats :: !Int
    }

instance FromNamedRecord District where
    parseNamedRecord r = District <$> r .: "identifier" <*> r .: "number_of_seats"

readDistricts :: FilePath -> IO()
readDistricts path = do
    csvData <- BL.readFile path
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, v) -> V.forM_ v $ \ p ->
            putStrLn $ identifier p ++ " contributes " ++ show (number_of_seats p) ++ " seats"

main :: IO()
main = do
    args <- getArgs
    case args of 
        [file] -> readDistricts file
        _ -> putStrLn "Wrong number of arguments"
