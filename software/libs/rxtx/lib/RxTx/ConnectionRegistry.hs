{-# LANGUAGE LambdaCase #-}

module RxTx.ConnectionRegistry (
  AsyncConnectionRegistry,
  mkConnectionRegistry,
  addConnection,
  removeConnection,
  killConnections,
) where

import Control.Concurrent (killThread)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar)
import Control.Monad (forM_)
import Data.Map qualified as Map
import RxTx.Connection (
  AsyncSomeConnection (..),
  Connection (cleanup),
  SomeConnection (..),
 )

newtype AsyncConnectionRegistry id msg
  = AsyncConnectionRegistry (MVar (Map.Map id (AsyncSomeConnection msg)))

mkConnectionRegistry :: IO (AsyncConnectionRegistry id msg)
mkConnectionRegistry = do
  mapMVar <- newMVar Map.empty
  pure $ AsyncConnectionRegistry mapMVar

addConnection ::
  Ord id =>
  id
  -> AsyncSomeConnection msg
  -> AsyncConnectionRegistry id msg
  -> IO ()
addConnection iden asyncConn (AsyncConnectionRegistry mvarMap) = do
  modifyMVar_ mvarMap $
    pure . Map.insert iden asyncConn

removeConnection ::
  Ord id => id -> AsyncConnectionRegistry id msg -> IO ()
removeConnection iden (AsyncConnectionRegistry mvarMap) = do
  modifyMVar_ mvarMap $
    Map.alterF
      ( \case
          Just (AsyncSomeConnection threadId (SomeConnection conn)) -> do
            cleanup conn
            killThread threadId
            pure Nothing
          Nothing -> pure Nothing
      )
      iden

killConnections :: AsyncConnectionRegistry id msg -> IO ()
killConnections (AsyncConnectionRegistry mvarMap) = do
  modifyMVar_ mvarMap $
    \connections -> do
      forM_ connections $ \(AsyncSomeConnection threadId (SomeConnection conn)) -> do
        cleanup conn
        killThread threadId
      pure Map.empty
