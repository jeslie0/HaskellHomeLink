{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Rest.Server (app)
import Data.Text.IO qualified as T

main :: IO ()
main = do
  T.putStrLn "http://localhost:8080/"
  run 8080 app
