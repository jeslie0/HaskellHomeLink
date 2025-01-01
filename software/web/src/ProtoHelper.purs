module ProtoHelper (toEither, class FromMessage, fromMessage, class SayError, sayError, class ToMessage, toMessage) where

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

toEither :: forall a b. b -> Maybe a -> Either b a
toEither _ (Just a) = Right a
toEither b _ = Left b

class FromMessage msg type_ err | msg -> err where
  fromMessage :: msg -> Either err type_

class ToMessage msg type_ where
  toMessage :: type_ -> msg

class SayError err where
  sayError :: err -> Array String
