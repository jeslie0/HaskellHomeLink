{-# LANGUAGE OverloadedStrings #-}

module Error.Parser (ParserError (..)) where

import Control.Exception (Exception (..))

data ParserError
  = ParserFailed String
  | ParserPartial
  deriving (Show)

instance Exception ParserError where
  displayException (ParserFailed str) = "Parser error: " <> str
  displayException ParserPartial = "Parser partially completed text"
