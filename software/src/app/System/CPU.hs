{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module System.CPU (
    CPUData (..),
    CPUDataError (..),
    modelName,
    getCPUData,
) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.Map.Strict qualified as Map
import Data.ProtoLens (defMessage)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Lens.Micro ((&), (.~), (^.))
import Lens.Micro.TH (makeLenses)
import Proto.Messages qualified as Proto
import Proto.Messages_Fields qualified as Proto
import ProtoHelper (FromMessage (..), ToMessage (..))

-- Types for key-value pairs
type Key = T.Text
type Value = T.Text
type KeyValuePair = (Key, Value)

-- | Parse a single key-value pair separated by comma
keyValueParser :: Parser KeyValuePair
keyValueParser = do
    key <- takeWhile1 (/= ':')
    _ <- char ':'
    value <- takeTill (== '\n')
    endOfLine <|> endOfInput
    return (T.strip key, T.strip value)

-- | Parse the entire file looking for desired keys
parseFileForKeys :: [Key] -> Parser (Map.Map Key Value)
parseFileForKeys desiredKeys = go Map.empty
  where
    go acc
        | all (`Map.member` acc) desiredKeys = return acc
        | otherwise = do
            skipSpace
            end <- atEnd
            if end
                then return acc
                else do
                    (key, value) <- keyValueParser
                    let newAcc =
                            if key `elem` desiredKeys
                                then Map.insert key value acc
                                else acc
                    go newAcc

-- | Top-level function to parse a file and extract desired key-value pairs
parseKeyValuePairFile :: FilePath -> [Key] -> IO (Maybe (Map.Map Key Value))
parseKeyValuePairFile filePath desiredKeys = do
    content <- TIO.readFile filePath
    let result = parseOnly (parseFileForKeys desiredKeys) content
    case result of
        Left _ -> pure Nothing
        Right kvMap -> pure $ Just kvMap

newtype CPUData = CPUData
    { _modelName :: T.Text
    }
    deriving (Show, Eq)

$(makeLenses ''CPUData)

data CPUDataError = NoModelName

instance ToMessage Proto.CPUData CPUData where
    toMessage cpuData =
        defMessage
            & Proto.modelName
            .~ (cpuData ^. modelName)

instance FromMessage Proto.CPUData CPUData where
    fromMessage msg = do
        CPUData
            { _modelName = msg ^. Proto.modelName
            }

getCPUData :: IO (Maybe CPUData)
getCPUData = do
    mMap <- parseKeyValuePairFile "/proc/cpuinfo" ["model name"]
    case mMap of
        Nothing -> putStrLn "Couldn't get cpuinfo" >> pure Nothing
        Just cpuDataMap -> do
            pure $
                CPUData <$> cpuDataMap Map.!? "model name"
