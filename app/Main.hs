module Main where

import PipeWire
import Data.Text qualified as T
import Data.Text.IO qualified as T

main :: IO ()
main = withPipeWire $ do
  mClie <- getClientName
  print mClie
