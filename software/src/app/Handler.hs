{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Handler (HasHandler(..), MaybeDecodeAndHandle(..)) where

import Data.ByteString qualified as B
import Data.Kind (Constraint, Type)
import Msg (Msg (..))

-- | The handler type class. Instances have a canonical way of being
-- handled.
class HasHandler a where
  handle :: a -> IO ()

-- | This type family allows us to constrain a type level list to
-- guarantee that it's entries all satisfy the HasHandler constraint.
type family AllHasHandler (xs :: [Type]) :: Constraint where
  AllHasHandler '[] = ()
  AllHasHandler (x ': xs) = (HasHandler x, AllHasHandler xs)

-- | This class gives us the ability to try to decode a bytestring to
-- any of the types listed in xs.
class MaybeDecodeAndHandle (xs :: [Type]) where
  maybeDecodeAndHandle :: B.ByteString -> Maybe (IO ())

-- | The base case for pattern matching.
instance MaybeDecodeAndHandle '[] where
  maybeDecodeAndHandle _ = Nothing

-- | The inductive case. We force x to have a handler, be a message
-- and require the tail of the list to also be constrained.
instance (HasHandler x, Msg x, MaybeDecodeAndHandle xs) => MaybeDecodeAndHandle (x ': xs) where
  maybeDecodeAndHandle bytes =
    case fromBytes @x bytes of
      Left _ -> maybeDecodeAndHandle @xs bytes
      Right msg -> Just . handle $ msg
