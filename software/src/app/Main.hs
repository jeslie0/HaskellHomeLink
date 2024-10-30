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

envelope :: P.WrapperMsg
envelope =
    defMessage
        & P.m1
        .~ person

makeLenses ''Atom
makeLenses ''Point

shiftAtomX :: Atom -> Atom
shiftAtomX = (point . x) %~ (+ 1)

type Messages = '[P.Person]

main :: IO ()
main = do
    loop@(EventLoop addMsg killLoop) <- mkEventLoop
    go loop
  where
    go = undefined

-- where
--   mDecodeAndAdd ::
--     forall (xs :: [Type]).
--     (AllHaveHandler xs, MaybeDecodeHandleable xs) =>
--     (forall msg. (HasHandler msg, Msg msg) => msg -> IO ()) ->
--     B.ByteString ->
--     IO ()
--   mDecodeAndAdd addMsg bytes =
--     case maybeDecodeH @xs bytes of
--       Nothing -> putStrLn "failed to decode bytes!"
--       Just (HandleableMsg msg) -> addMsg msg

--   go loop@(EventLoop addMsg killLoop) = do
--     let pBytes = toBytes person
--     mDecodeAndAdd @Messages addMsg pBytes

--     let aBytes = toBytes address
--     mDecodeAndAdd @Messages addMsg aBytes
