{-# LANGUAGE OverloadedStrings #-}

module Error.Parser (ParserError (..), parseOnly) where

import Control.Exception (Exception (..))
import Data.Attoparsec.Text qualified as Atto
import qualified Data.Text as T
import Error (ErrorT, Error (..))

data ParserError
  = ParserFailed String
  | ParserPartial
  deriving (Show)

instance Exception ParserError where
  displayException (ParserFailed str) = "Parser error: " <> str
  displayException ParserPartial = "Parser partially completed text"

parseOnly :: Atto.Parser a -> T.Text -> Either Error a
parseOnly parser txt =
    case Atto.parseOnly parser txt of
    Left str -> Left . Error . ParserFailed $ str
    Right lsts -> Right lsts
