{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Msg (
  Msg (..),
  ExMsg (..),
  MaybeDecode (..),
) where

import Data.ByteString qualified as B
import Data.Kind (Type)
import Data.ProtoLens (Message)
import Data.ProtoLens.Encoding (decodeMessage, encodeMessage)
import Data.Serialize (get, put, runGet, runPut)
import Islands (Island)

class Msg msg where
  toBytes :: msg -> B.ByteString

  fromBytes :: B.ByteString -> Either String msg

instance {-# OVERLAPPING #-} Msg msg => Msg (Island, Island, msg) where
  toBytes (src, dest, msg) =
    runPut (put src) <> runPut (put dest) <> toBytes msg

  fromBytes bytes = do
    (src, dest) <-
      runGet
        ( do
            src <- get @Island
            dest <- get @Island
            pure (src, dest)
        )
        bytes
    msg <- fromBytes $ B.drop 2 bytes
    pure (src, dest, msg)

instance {-# OVERLAPPING #-} Message msg => Msg msg where
  toBytes = encodeMessage

  fromBytes = decodeMessage

data ExMsg = forall msg. Msg msg => ExMsg msg

-- | This class gives us the ability to try to decode a bytestring to
-- any of the types listed in xs.
class MaybeDecode (xs :: [Type]) where
  maybeDecode :: B.ByteString -> Maybe ExMsg

-- | The base case for pattern matching.
instance MaybeDecode '[] where
  maybeDecode _ = Nothing

-- | The inductive case. We force x to have a handler, be a message
-- and require the tail of the list to also be constrained.
instance (Msg x, MaybeDecode xs) => MaybeDecode (x ': xs) where
  maybeDecode bytes =
    case fromBytes @x bytes of
      Left _ -> maybeDecode @xs bytes
      Right msg -> Just $ ExMsg msg
