{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Msg (Msg (..), AllHasMsg) where

import Data.ByteString qualified as B
import Data.ProtoLens (Message)
import Data.ProtoLens.Encoding (decodeMessage, encodeMessage)
import Data.Kind (Type, Constraint)

class Msg msg where
  toBytes :: msg -> B.ByteString

  fromBytes :: B.ByteString -> Either String msg

instance (Message msg) => Msg msg where
  toBytes = encodeMessage

  fromBytes = decodeMessage

type family AllHasMsg (xs :: [Type]) :: Constraint where
  AllHasMsg '[] = ()
  AllHasMsg (x ': xs) = (Msg x, AllHasMsg xs)
