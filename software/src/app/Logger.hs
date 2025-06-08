{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Logger (
  Logs,
  LogLevel (..),
  mkLogs,
  getLogs,
  addLog,
  reportLog,
) where

import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar)
import Data.Foldable (forM_)
import Data.ProtoLens (defMessage)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time.Clock.System (SystemTime (..), getSystemTime, systemToUTCTime)
import Data.Time.Format
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Devices (Device, proxies)
import Envelope (wrapProxyMsg)
import Lens.Micro ((&), (.~), (^.))
import Proto.DeviceData qualified as Proto
import Proto.Logging qualified as Proto
import Proto.Logging_Fields qualified as Proto
import ProtoHelper (FromMessage (..), ToMessage (..))
import Router (Router, thisDevice, trySendMessage)

data LogLevel
  = Trace
  | Debug
  | Info
  | Warn
  | Error
  | Fatal
  | Unknown

instance ToMessage Proto.LOG_LEVEL LogLevel where
  toMessage Trace = Proto.TRACE
  toMessage Debug = Proto.DEBUG
  toMessage Info = Proto.INFO
  toMessage Warn = Proto.WARN
  toMessage Error = Proto.ERROR
  toMessage Fatal = Proto.FATAL
  toMessage Unknown = Proto.FATAL

newtype Logs = Logs (MVar (V.Vector Proto.Log))

mkLogs :: IO Logs
mkLogs = do
  logsRef <- newMVar V.empty
  pure $ Logs logsRef

getLogs :: Logs -> IO (V.Vector Proto.Log)
getLogs (Logs ref) = readMVar ref

addLog :: Logs -> Proto.Log -> IO ()
addLog (Logs logsRef) newLog =
  modifyMVar_ logsRef $ \logs -> do
    if V.length logs <= maxLogCount
      then
        pure $ V.snoc logs newLog
      else do
        mVec <- V.unsafeThaw logs
        forM_ [0 .. VM.length mVec - 2] $ \(i :: Int) ->
          VM.read mVec (i + 1) >>= VM.write mVec i
        VM.write mVec (VM.length mVec - 1) newLog
        V.unsafeFreeze mVec
 where
  maxLogCount = 1000

formatLog :: Proto.Log -> T.Text
formatLog log' =
  let
    island =
      T.pack (show $ fromMessage @Proto.DEVICE @Device $ log' ^. Proto.device)
    time =
      T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" $
        systemToUTCTime (MkSystemTime (log' ^. Proto.timestampMs `div` 1000) 0)
  in
    "[" <> time <> "] [" <> island <> "] " <> (log' ^. Proto.content)

reportLog ::
  Router -> LogLevel -> T.Text -> IO ()
reportLog rtr lvl txt = do
  MkSystemTime sec nano <- getSystemTime
  let
    log' =
      defMessage
        & Proto.timestampMs
        .~ (1000 * sec + fromIntegral nano `div` 1000000)
        & Proto.device
        .~ toMessage (rtr ^. thisDevice)
        & Proto.level
        .~ toMessage lvl
        & Proto.content
        .~ txt
    msg :: Proto.AddLog =
      defMessage & Proto.log .~ log'
  T.putStrLn . formatLog $ log'
  forM_ proxies $ \i ->
    trySendMessage rtr i $ wrapProxyMsg msg
