{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module : Home.Handler
-- Description : Defines the typeclass and instance for messages being
-- handled by the Home application.
module Home.Handler (
  HomeHandler (..),
  ExHomeHandler (..),
) where

import Control.Concurrent (forkFinally, forkIO, killThread)
import Control.Exception (bracketOnError)
import Control.Monad (void)
import Control.Monad.Reader
import Data.Foldable (forM_, traverse_)
import Data.IORef (atomicModifyIORef')
import Data.ProtoLens (defMessage)
import Data.Text qualified as T
import Data.Typeable (cast)
import Envelope (ToProxyEnvelope (..))
import EventLoop (
  EventLoopT,
  MonadEventLoop (..),
  addMsg,
  addMsgIO,
  getLoop,
  setTimeoutIO,
 )
import Home.AudioStream (startAudioStream)
import Home.AudioStreamTypes (StationId, StreamStatus (..))
import Home.Env (Env, audioStreamRef, router)
import Islands (Island (..), islands, proxies)
import Lens.Micro ((&), (.~), (?~), (^.), _1, _2, _3)
import Logger (LogLevel (..), reportLog)
import Network.Socket (AddrInfo (addrAddress), Socket, close)
import Proto.Messages qualified as Proto
import Proto.Messages_Fields qualified as Proto
import ProtoHelper (ToMessage (..))
import Router (
  Routable (..),
  Router,
  connectionsRegistry,
  tryForwardMessage,
  trySendMessage,
 )
import RxTx.Connection (addSub, cleanup, mkConnection, recvAndDispatch)
import RxTx.Connection.Socket (aquireClientSocket, connectToHost)
import RxTx.ConnectionRegistry (addConnection)
import RxTx.Socket (SocketRxError (..), SocketTxError)
import System.Memory (getMemoryInformation)
import TH (makeInstance)

class HomeHandler msg where
  homeHandler ::
    Island -> msg -> EventLoopT Env (Island, ExHomeHandler) IO ()

data ExHomeHandler = forall a. HomeHandler a => ExHomeHandler a

instance HomeHandler ExHomeHandler where
  homeHandler island (ExHomeHandler msg) = homeHandler island msg

-- * Message instances

-- Make HomeHandler Home.Envelope instance.
$( makeInstance
    ''HomeHandler
    ''Proto.HomeEnvelope
    'Proto.maybe'payload
    ''Proto.HomeEnvelope'Payload
 )

notifyProxyRadioStatus ::
  Router -> Island -> StreamStatus -> StationId -> IO Bool
notifyProxyRadioStatus rtr island status stationId =
  trySendMessage
    rtr
    island
    ( toProxyEnvelope $
        defMessage @Proto.RadioStatusUpdate
          & Proto.status
          .~ toMessage status
          & Proto.currentStationId
          .~ stationId
    )

-- | Try to make a new asynchronous audio stream in a separate
-- thread. If one exists, report the error.
instance HomeHandler Proto.ModifyRadioRequest where
  homeHandler src req = do
    env <- getEnv
    let notify = do
          (_, status, stationId) <- atomicModifyIORef' (env ^. audioStreamRef) $ \a -> (a, a)
          void $ notifyProxyRadioStatus (env ^. router) src status stationId
    (mThreadId, _, _) <- liftIO . atomicModifyIORef' (env ^. audioStreamRef) $ \a -> (a, a)
    case (mThreadId, req ^. Proto.maybe'station) of
      (Just _, Just _) -> do
        liftIO $ reportLog (env ^. router) Info "Audio stream already exists"
      (Just threadId, Nothing) -> do
        liftIO $ killThread threadId
      (Nothing, Nothing) -> liftIO $ reportLog (env ^. router) Info "Radio not playing"
      (Nothing, Just station) -> do
        let updateStreamStatus status = do
              atomicModifyIORef' (env ^. audioStreamRef) $ \st -> (st, ()) & _1 . _2 .~ status
              notify
        threadId <-
          liftIO $
            forkFinally
              (startAudioStream (env ^. router) (station ^. Proto.url) updateStreamStatus)
              ( \_ -> do
                  atomicModifyIORef' (env ^. audioStreamRef) $ \st ->
                    (st, ())
                      & _1
                      . _1
                      .~ Nothing
                      & _1
                      . _2
                      .~ Off
                  void notify
                  reportLog (env ^. router) Info "Stopping radio stream"
              )
        liftIO
          . atomicModifyIORef'
            (env ^. audioStreamRef)
          $ \st ->
            (st, ())
              & _1
              . _1
              ?~ threadId
              & _1
              . _3
              .~ (station ^. Proto.id)
    void . liftIO $ notify

-- | Spawn a new thread and try to connect to the given TCP server.
instance HomeHandler Proto.ConnectTCP where
  homeHandler _ msg = do
    env <- getEnv
    loop <- getLoop
    let
      host = msg ^. Proto.host
      port = msg ^. Proto.port
      registry = env ^. router . connectionsRegistry

    void . liftIO $
      bracketOnError
        (aquireClientSocket (T.unpack host) (T.unpack port))
        (traverse_ $ \(sock, _) -> close sock)
        (withSocket loop registry)
   where
    withSocket loop _ Nothing =
      void $ setTimeoutIO (Home, ExHomeHandler msg) 2000 loop
    withSocket loop registry (Just (sock, addr)) = do
      success <- connectToHost sock addr
      if success
        then do
          putStrLn $ "Connected to " <> show (addrAddress addr)
          conn <- mkConnection @Routable @Socket @SocketRxError @SocketTxError sock close
          addSub conn $ \rtrMsg -> do
            case parseMessage rtrMsg of
              Nothing -> putStrLn "Failed to parse message!"
              Just homeMsg -> addMsgIO (Home, ExHomeHandler homeMsg) loop
          threadId <- forkIO . void $ runConnection conn loop
          addConnection RemoteProxy threadId conn registry
        else do
          putStrLn $
            "Failed to connect to server: "
              <> show (addrAddress addr)
              <> ". Trying again in 2s..."
          void $ setTimeoutIO (Home, ExHomeHandler msg) 2000 loop

    parseMessage :: Routable -> Maybe Proto.HomeEnvelope
    parseMessage (Routable rmsg) =
      cast rmsg :: Maybe Proto.HomeEnvelope

    runConnection conn loop = do
      merror <- recvAndDispatch conn
      case merror of
        Nothing -> runConnection conn loop
        Just (SocketRxError _ err) -> do
          print $ "ERROR: " <> show err
          cleanup conn
          void $ setTimeoutIO (Home, ExHomeHandler msg) 2000 loop

--
instance HomeHandler Proto.SystemData where
  homeHandler _ msg = do
    env <- getEnv
    liftIO . forM_ (filter (/= Home) islands) $ \island ->
      trySendMessage (env ^. router) island msg

instance HomeHandler Proto.MemoryInformation where
  homeHandler src msg = do
    env <- getEnv
    forM_ proxies $ \proxy -> do
      liftIO $ do
        void . tryForwardMessage (env ^. router) src proxy $ toProxyEnvelope msg

instance HomeHandler Proto.CheckMemoryUsage where
  homeHandler _ _ = do
    env <- getEnv
    mMemInfo <- liftIO getMemoryInformation
    case mMemInfo of
      Nothing -> liftIO $ reportLog (env ^. router) Error "Failed to get memory info"
      Just memInfo -> do
        addMsg (Home, ExHomeHandler $ toMessage @Proto.MemoryInformation memInfo)
