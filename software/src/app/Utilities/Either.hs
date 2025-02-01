module Utilities.Either (toEither) where

toEither :: e -> Maybe a -> Either e a
toEither err Nothing = Left err
toEither _ (Just a) = Right a
