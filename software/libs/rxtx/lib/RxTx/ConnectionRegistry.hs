{-# LANGUAGE LambdaCase #-}

module RxTx.ConnectionRegistry (
  AsyncConnectionRegistry (..),
  mkConnectionRegistry,
  addConnection,
  addSomeConnection,
  removeConnection,
  killConnections,
  RxTx.ConnectionRegistry.send,
) where

import Control.Concurrent (
  ThreadId,
  withMVar,
 )
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar)
import Control.Monad (forM_)
import Data.Map qualified as Map
import RxTx.Connection (
  AsyncSomeConnection (..),
  Connection (..),
  SomeConnection (..),
 )

newtype AsyncConnectionRegistry id msg
  = AsyncConnectionRegistry (MVar (Map.Map id (AsyncSomeConnection msg)))

mkConnectionRegistry :: IO (AsyncConnectionRegistry id msg)
mkConnectionRegistry = do
  mapMVar <- newMVar Map.empty
  pure $ AsyncConnectionRegistry mapMVar

addSomeConnection ::
  Ord id =>
  id
  -> AsyncSomeConnection msg
  -> AsyncConnectionRegistry id msg
  -> IO ()
addSomeConnection iden asyncConn (AsyncConnectionRegistry mvarMap) = do
  modifyMVar_ mvarMap $
    pure . Map.insert iden asyncConn

addConnection ::
  forall msg id rxErr txErr.
  Ord id =>
  id
  -> ThreadId
  -> Connection msg rxErr txErr
  -> AsyncConnectionRegistry id msg
  -> IO ()
addConnection iden threadId conn (AsyncConnectionRegistry mvarMap) = do
  modifyMVar_ mvarMap $
    pure . Map.insert iden (AsyncSomeConnection threadId (SomeConnection conn))

removeConnection ::
  Ord id => id -> AsyncConnectionRegistry id msg -> IO ()
removeConnection iden (AsyncConnectionRegistry mvarMap) = do
  modifyMVar_ mvarMap $
    Map.alterF
      ( \case
          Just (AsyncSomeConnection _ (SomeConnection conn)) -> do
            cleanup conn
            pure Nothing
          Nothing -> pure Nothing
      )
      iden

killConnections :: AsyncConnectionRegistry id msg -> IO ()
killConnections (AsyncConnectionRegistry mvarMap) = do
  modifyMVar_ mvarMap $
    \connections -> do
      forM_ connections $ \(AsyncSomeConnection _ (SomeConnection conn)) -> do
        cleanup conn
      pure Map.empty

send :: Ord id => AsyncConnectionRegistry id msg -> id -> msg -> IO Bool
send (AsyncConnectionRegistry mvarMap) iden msg = do
  withMVar mvarMap $ \connMap ->
    case Map.lookup iden connMap of
      Nothing -> pure False
      Just (AsyncSomeConnection _ (SomeConnection conn)) -> do
        eErr <- RxTx.Connection.send conn msg
        case eErr of
          Left _ -> pure False
          Right _ -> pure True

-- foo :: IO ()
-- foo = do
--   registry <- mkConnectionRegistry
--   print "made registry"
--   threadId <- forkIO $ recvThread registry
--   print "made thread"
--   sender registry
--   killThread threadId
--  where
--   recvThread registry = do
--     Just conn <- createConnection @B.ByteString "127.0.0.1" "8080"
--     print "CONNECTED!"
--     addSub conn $ \bytes -> print bytes
--     threadId <- myThreadId
--     print "got theadid"
--     addConnection (1 :: Int) threadId conn registry
--     print "added connection"
--     run conn
--    where
--     run conn = do
--       merror <- recvAndDispatch conn
--       case merror of
--         Nothing -> run conn
--         Just (SocketRxError _ err) -> do
--           print $ "ERROR: " <> show err
--           removeConnection 1 registry

--   sender registry = do
--     threadDelay 1000000
--     bytes <- C.getLine
--     success <- RxTx.ConnectionRegistry.send registry 1 bytes
--     if success
--       then sender registry
--       else print "FAILED TO SEND"
