{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Data.ProtoLens
import Lens.Micro
import Lens.Micro.TH
import Proto.Person as P
import Proto.Person_Fields as P
import Handler
import EventLoop

data Atom = Atom {_element :: String, _point :: Point}

data Point = Point {_x :: Double, _y :: Double}

person :: P.Person
person =
  defMessage
    & P.name .~ "James"
    & P.age .~ 29
    & P.addresses .~ []

address :: P.Address
address = defMessage
  & P.street .~ "foo"
  & P.zipCode .~ "EH3 0SE"

makeLenses ''Atom
makeLenses ''Point

shiftAtomX :: Atom -> Atom
shiftAtomX = (point . x) %~ (+ 1)

instance HasHandler P.Person where
  handle = print

instance HasHandler P.Address where
  handle = print

main :: IO ()
main = do
  loop@(EventLoop addMsg killLoop) <- mkEventLoop
  addMsg person
  addMsg address
  go loop
  where
    go loop@(EventLoop addMsg killLoop) = do
      input <- getLine
      if
        | input == "kill" -> killLoop
        | input == "person" -> addMsg person >> go loop
        | input == "address" -> addMsg address >> go loop
        | otherwise -> go loop
