{-# LANGUAGE OverloadedStrings #-}

module Test where

import Control.Concurrent
import Control.Exception (SomeException, catch)
import Control.Monad (forever, void, when)
import Data.Foldable (forM_)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Network.WebSockets qualified as WS
import System.Random (randomIO)

type ConnectionStore = [(Int, WS.Connection)]

wsServer :: MVar ConnectionStore -> IO ()
wsServer mMap = do
  putStrLn "Starting WS server on port 3000"
  WS.runServer "0.0.0.0" 3000 $ \pendingConn -> do
    conn <- WS.acceptRequest pendingConn
    (n :: Int) <- randomIO
    putStrLn $ "[wsServer]: Accepted connection - id " <> show n
    WS.sendTextData conn ("Initial message" :: T.Text)
    loop n conn
 where
  loop id conn = do
    res <-
      (WS.receive conn >> pure True) `catch` \(e :: SomeException) -> do
        putStrLn "[wsServer]: Connection terminated"
        modifyMVar_ mMap $ \store -> pure . filter ((/= id) . fst) $ store
        pure False
    when res $ loop id conn

broadcast :: T.Text -> ConnectionStore -> IO ()
broadcast txt mMap = do
  forM_
    mMap
    ( \(idn, conn) -> do
        WS.sendTextData conn txt
          `catch` ( \(e :: SomeException) -> do
                      putStrLn $ "An exception happened and was caught: " <> show e
                  )
    )

mainLoop :: MVar ConnectionStore -> IO ()
mainLoop mMap =
  go
 where
  go = do
    str <- T.getLine
    withMVar mMap $ broadcast str
    go

main :: IO ()
main = do
  mMap <- newMVar []
  serverThread <- forkIO $ wsServer mMap
  mainLoop mMap
