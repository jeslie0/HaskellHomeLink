{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module System.CPU where

import Data.Aeson (ToJSON)
import Data.Attoparsec.Text (Parser, char, many1', skipSpace, takeTill)
import Data.Char (isSpace)
import Data.List (find)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Error (ErrorT (..), fromMaybe, toEitherErrorStack, tryError, withErrorStack, toErrorT, withError, toError)
import Error.Generic (GenericError (..))
import Error.Parser (parseOnly)
import GHC.Generics (Generic)

cpuDataPath :: FilePath
cpuDataPath = "/proc/cpuinfodfads"

cpuDataText :: IO (ErrorT T.Text)
cpuDataText =
  toErrorT <$> (tryError @IOError . T.readFile $ cpuDataPath)

keyValueParser :: Parser (T.Text, T.Text)
keyValueParser = do
  skipSpace
  key <- takeTill (== ':')
  _ <- char ':'
  skipSpace
  value <- takeTill (== '\n')
  return (T.filter (not . isSpace) key, T.filter (not . isSpace) value)

cpuDataExtract :: IO (ErrorT [(T.Text, T.Text)])
cpuDataExtract = do
  withErrorStack (parseOnly (many1' keyValueParser)) <$> cpuDataText

data CPUData = CPUData {modelName :: T.Text} deriving (Generic)

instance ToJSON CPUData

getCPUData :: IO (ErrorT CPUData)
getCPUData = do
  extract <- cpuDataExtract
  return $
    CPUData . snd <$> withErrorStack (toError . fromMaybe FailedToFindElement . find (\(key, _) -> key == "modelname")) extract
