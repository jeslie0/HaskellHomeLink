module Parsing (
  keyValueParser,
  parseFileForKeys,
  parseKeyValuePairFile,
  Key,
  Value,
  KeyValuePair,
) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as T

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
  content <- T.readFile filePath
  let result = parseOnly (parseFileForKeys desiredKeys) content
  case result of
    Left _ -> pure Nothing
    Right kvMap -> pure $ Just kvMap
