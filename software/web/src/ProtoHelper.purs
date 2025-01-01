module ProtoHelper (toEither, class FromMessage, fromMessage, class SayError, sayError) where

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

toEither :: forall a b. b -> Maybe a -> Either b a
toEither _ (Just a) = Right a
toEither b _ = Left b

class FromMessage msg type_ err | msg -> err where
  fromMessage :: msg -> Either err type_

class SayError err where
  sayError :: err -> Array String
