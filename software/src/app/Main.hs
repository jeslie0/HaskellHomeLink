{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.ByteString qualified as B
import Data.Kind (Type)
import Data.ProtoLens
import Data.ProtoLens.Encoding
import Data.ProtoLens.Encoding.Parser
import Data.Proxy
import EventLoop
import Lens.Micro
import Lens.Micro.TH
import Msg
import Proto.Person as P
import Proto.Person_Fields as P
import TH

data Atom = Atom {_element :: String, _point :: Point}

data Point = Point {_x :: Double, _y :: Double}

makeLenses ''Atom
makeLenses ''Point

shiftAtomX :: Atom -> Atom
shiftAtomX = (point . x) %~ (+ 1)

person :: P.Person
person =
    defMessage
        & P.name
        .~ "James"
        & P.age
        .~ 29
        & P.addresses
        .~ []

address :: P.Address
address =
    defMessage
        & P.street
        .~ "foo"
        & P.zipCode
        .~ "EH3 0SE"

pEnv :: P.WrapperMsg
pEnv =
    defMessage
        & P.m1
        .~ person

aEnv :: P.WrapperMsg
aEnv =
    defMessage
        & P.m2
        .~ address


type Messages = '[P.Person]

foo :: P.WrapperMsg -> ExCoreHandlable
foo (P.Envelope'M1 person) = handle person

$(generateHandler )
-- main :: IO ()
-- main =
foo =
  let
    personBytes = toBytes pEnv
    addrBytes = toBytes aEnv
  in
    maybeDecode @Messages addrBytes
