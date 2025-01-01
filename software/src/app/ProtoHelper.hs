{-# LANGUAGE FunctionalDependencies #-}

module ProtoHelper (
    FromMessage (..),
    ToMessage (..),
    toEither,
) where

toEither :: forall a b. b -> Maybe a -> Either b a
toEither _ (Just a) = Right a
toEither b _ = Left b

class FromMessage msg type_ where
    fromMessage :: msg -> type_

class ToMessage msg type_ where
    toMessage :: type_ -> msg
