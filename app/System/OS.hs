{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module System.OS (getOSInfo, OSInfo (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Attoparsec.Text (IResult (..), Parser, char, endOfInput, many', manyTill, parse, skipSpace, takeTill, string)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Error.Parser (ParserError (..))
import GHC.Generics (Generic)

data OSInfo = OSInfo
  { name :: {-# UNPACK #-} !T.Text,
    prettyName :: {-# UNPACK #-} !T.Text
  }
  deriving (Generic, Show)

instance ToJSON OSInfo

instance FromJSON OSInfo

data OSInformationType =
  PrettyName T.Text
  | Name T.Text
  | Other

prettyNameParser :: Parser OSInformationType
prettyNameParser = do
  _ <- char '"'
  _ <- string "PRETTY_NAME"
  _ <- char '"'
  _ <- char '='
  value <- takeTill ('\n' ==)
  _ <- char '\n'
  return . PrettyName $ T.filter (/= '"') value

nameParser :: Parser OSInformationType
nameParser = do
  _ <- char '"'
  _ <- string "NAME"
  _ <- char '"'
  _ <- char '='
  value <- takeTill ('\n' ==)
  _ <- char '\n'
  return . PrettyName $ T.filter (/= '"') value

otherParser :: Parser OSInformationType
otherParser = do
  _ <- takeTill ('\n' ==)
  _ <- char '\n'
  return Other

osReleasePath :: FilePath
osReleasePath =
  "/etc/os-release"

osRelease :: IO T.Text
osRelease =
  T.readFile osReleasePath

keyValueParser :: Parser (T.Text, T.Text)
keyValueParser = do
  key <- takeTill ('=' ==)
  _ <- char '='
  value <- takeTill ('\n' ==)
  _ <- char '\n'
  return (T.filter (/= '"') key, T.filter (/= '"') value)

assocListParser :: Parser [(T.Text, T.Text)]
assocListParser = manyTill keyValueParser endOfInput

getOSInfo :: IO (Either ParserError OSInfo)
getOSInfo = do
  result <- parse assocListParser <$> osRelease
  go result
  where
    go result =
      case result of
        Fail _ _ err -> return . Left . ParserFailed $ err
        Partial f -> go (f "")
        Done _ assocList ->
          let nameMaybe = lookup "NAME" assocList
              prettyNameMaybe = lookup "PRETTY_NAME" assocList
           in case (nameMaybe, prettyNameMaybe) of
                (Just name, Just prettyName) -> return . Right $ OSInfo name prettyName
                _ -> return . Left . ParserFailed $ "Couldn't lookup data"
