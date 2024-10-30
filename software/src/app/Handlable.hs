{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Handlable
  ( HasHandler (..),
    HandleableMsg (..),
    AllHaveHandler,
    decodeEnvelope,
  )
where

import Data.ByteString qualified as B
import Data.Kind (Constraint, Type)
import Lens.Micro

-- | The handler type class. Instances have a canonical way of being
-- handled.
class HasHandler a where
  handle :: a -> IO ()

-- | This type family allows us to constrain a type level list to
-- guarantee that it's entries all satisfy the HasHandler constraint.
type family AllHaveHandler (xs :: [Type]) :: Constraint where
  AllHaveHandler '[] = ()
  AllHaveHandler (x ': xs) = (HasHandler x, AllHaveHandler xs)

-- | This class gives us the ability to try to decode a bytestring to
-- any of the types listed in xs.
class MaybeDecodeHandleable (xs :: [Type]) where
  maybeDecodeH :: B.ByteString -> Maybe HandleableMsg

-- | The base case for pattern matching.
instance MaybeDecodeHandleable '[] where
  maybeDecodeH _ = Nothing

-- | The inductive case. We force x to have a handler, be a message
-- and require the tail of the list to also be constrained.
instance (HasHandler x, Msg x, MaybeDecodeHandleable xs) => MaybeDecodeHandleable (x ': xs) where
  maybeDecodeH bytes =
    case fromBytes @x bytes of
      Left _ -> maybeDecodeH @xs bytes
      Right msg -> Just $ HandleableMsg msg

instance HasHandler Person where
  handle = print

envelopeToHandleable :: WrapperMsg'Msg -> HandleableMsg
envelopeToHandleable (WrapperMsg'M1 x_a3V3t) =
  HandleableMsg x_a3V3t

decodeEnvelope :: B.ByteString -> Either String HandleableMsg
decodeEnvelope bytes = do
  envelope <- fromBytes @WrapperMsg bytes
  case envelope ^. maybe'msg of
    Nothing -> Left "No msg"
    Just msg -> return . envelopeToHandleable $ msg
