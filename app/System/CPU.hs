{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module System.CPU where

import Data.Aeson (ToJSON)
import Data.Attoparsec.Text (Parser, char, many1', parseOnly, skipSpace, string, takeLazyText, takeTill)
import Data.Char (isSpace)
import Data.List (find)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Error (ErrorStack (..), ErrorType (..), fromMaybe, toEitherErrorStack)
import Error.Generic (GenericError (..))
import Error.Parser (ParserError (..))
import GHC.Generics (Generic)

cpuDataPath :: FilePath
cpuDataPath = "/proc/cpuinfo"

cpuDataText :: IO T.Text
cpuDataText = T.readFile cpuDataPath

keyValueParser :: Parser (T.Text, T.Text)
keyValueParser = do
  skipSpace
  key <- takeTill (== ':')
  _ <- char ':'
  skipSpace
  value <- takeTill (== '\n')
  return (T.filter (not . isSpace) key, T.filter (not . isSpace) value)

cpuDataExtract :: IO (Either ParserError [(T.Text, T.Text)])
cpuDataExtract = do
  txt <- cpuDataText
  return $ case parseOnly (many1' keyValueParser) txt of
    Left str -> Left . ParserFailed $ str
    Right lsts -> Right lsts

data CPUData = CPUData {modelName :: T.Text} deriving (Generic)

instance ToJSON CPUData

getCPUData :: IO (Either ErrorStack CPUData)
getCPUData = do
  extract <- cpuDataExtract
  return $ do
    alist <- toEitherErrorStack extract
    (_, val) <- toEitherErrorStack $ fromMaybe FailedToFindElement $ find (\(key, _) -> key == "modelname") alist
    return $ CPUData {modelName = val}
