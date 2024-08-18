{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module System.OS (getOSInfo, OSInfo (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Attoparsec.Text (IResult (..), Parser, char, endOfInput, manyTill, parse, takeTill)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Error.Parser (ParserError (..))
import GHC.Generics (Generic)
import Error (tryError, Error(..))

data OSInfo = OSInfo
  { name :: {-# UNPACK #-} !T.Text,
    prettyName :: {-# UNPACK #-} !T.Text
  }
  deriving (Generic, Show)

instance ToJSON OSInfo

instance FromJSON OSInfo

osReleasePath :: FilePath
osReleasePath =
  "/etc/os-releasefdsafdsafs"

osRelease :: IO (Either Error T.Text)
osRelease =
  tryError @IOError $ T.readFile osReleasePath

keyValueParser :: Parser (T.Text, T.Text)
keyValueParser = do
  key <- takeTill ('=' ==)
  _ <- char '='
  value <- takeTill ('\n' ==)
  _ <- char '\n'
  return (T.filter (/= '"') key, T.filter (/= '"') value)

assocListParser :: Parser [(T.Text, T.Text)]
assocListParser = manyTill keyValueParser endOfInput

getOSInfo :: IO (Either Error OSInfo)
getOSInfo = do
  txtEither <- osRelease
  return $ do
    txt <- txtEither
    let result = parse assocListParser txt
    case go result of
      Left err -> Left (Error err)
      Right a -> Right a
  where
    go result =
      case result of
        Fail _ _ err -> Left $ ParserFailed err
        Partial f -> go (f "")
        Done _ assocList ->
          let nameMaybe = lookup "NAME" assocList
              prettyNameMaybe = lookup "PRETTY_NAME" assocList
           in case (nameMaybe, prettyNameMaybe) of
                (Just name, Just prettyName) -> Right $ OSInfo name prettyName
                _ -> Left $ ParserFailed "Couldn't lookup data"
