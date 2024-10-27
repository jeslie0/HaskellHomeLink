{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.ProtoLens
import Lens.Micro
import Lens.Micro.TH
import Proto.Person as P
import Proto.Person_Fields as P

data Atom = Atom {_element :: String, _point :: Point}

data Point = Point {_x :: Double, _y :: Double}

makeLenses ''Atom
makeLenses ''Point

shiftAtomX :: Atom -> Atom
shiftAtomX = (point . x) %~ (+ 1)

main :: IO ()
main = do
  putStrLn "Hello, World!"
  let atom = Atom "test" (Point 0.0 0.0)
      newAtom = shiftAtomX atom
  print $ newAtom ^. point . x
  print . showMessage $ person
