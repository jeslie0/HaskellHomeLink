{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ProtoHelper (
  FromMessage (..),
  ToMessage (..),
  toEither,
) where

import Data.ProtoLens (Message (..), decodeMessage)
import Data.ProtoLens.Encoding (encodeMessage)
import Data.Serialize (Serialize (..), getBytes, remaining)
import Data.Serialize.Put (putByteString)

toEither :: forall a b. b -> Maybe a -> Either b a
toEither _ (Just a) = Right a
toEither b _ = Left b

class FromMessage msg type_ where
  fromMessage :: msg -> type_

class ToMessage msg type_ where
  toMessage :: type_ -> msg

instance {-# OVERLAPPABLE #-} Message msg => Serialize msg where
  get = do
    bytesLeft <- remaining
    bytes <- getBytes bytesLeft
    case decodeMessage @msg bytes of
      Left str -> fail str
      Right msg -> pure msg

  put =
    putByteString . encodeMessage

