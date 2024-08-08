{-# LANGUAGE OverloadedStrings #-}
module System.CPU where

import Data.Aeson (FromJSON, ToJSON)
import Data.Attoparsec.Text (IResult (..), Parser, char, endOfInput, manyTill, parse, takeTill, string, satisfy, skipSpace, many1', manyTill')
import Data.Char (isSpace)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Error.Parser (ParserError (..))
import GHC.Generics (Generic)

cpuDataPath :: FilePath
cpuDataPath = "/proc/cpuinfo"

cpuDataText :: IO T.Text
cpuDataText = T.readFile cpuDataPath


keyValueParser :: Parser (T.Text, T.Text)
keyValueParser = do
  key <- takeTill (== ':')
  _ <- char ':'
  skipSpace
  value <- takeTill (== '\n')
  skipSpace
  return (T.filter (not . isSpace) key, T.filter (not . isSpace) value)

cpuBlockParser :: Parser [(T.Text, T.Text)]
cpuBlockParser = manyTill' keyValueParser (char '\n')

cpuParser :: Parser [[(T.Text, T.Text)]]
cpuParser = do
  block <- cpuBlockParser
  _ <- char '\n'
  rest <- cpuParser
  return $ block:rest


cpuData :: IO [[(T.Text, T.Text)]]
cpuData = do
  txt <- cpuDataText
  let res = parse cpuParser txt
  go res
  where
    go res =
        case res of
            Done _ lst ->  return lst
            Partial f -> go $ f ""
            Fail _ _ _ -> error ""
