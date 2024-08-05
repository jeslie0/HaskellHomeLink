{-# LANGUAGE OverloadedStrings #-}

module Error.Parser (ParserError (..)) where

import Data.Text qualified as T
import Error (Error (..))

data ParserError
  = ParserFailed String
  | ParserPartial


instance Error ParserError where
  errorString (ParserFailed str) = "Parser error: " <> T.pack str
  errorString ParserPartial = "Parser partially completed text"
