{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module : Home.Handler
-- Description : Defines the typeclass and instance for messages being
-- handled by the Home application.
module Home.Handler where

import Control.Monad.Except (ExceptT, runExceptT)
import Devices (Device)
import Envelope (wrapProxyMsg)
import HAsio.Error.ErrorStack
import Home.Env (Env)
import Lens.Micro ((&), (.~), (?~), (^.), _1, _2, _3)
import Network.Socket (AddrInfo (addrAddress), close)
import Network.TLS qualified as TLS
import Proto.DeviceData qualified as Proto
import Proto.Envelope qualified as Proto
import Proto.Envelope_Fields qualified as Proto
import Proto.Radio qualified as Proto
import Proto.Radio_Fields qualified as Proto
import ProtoHelper (ToMessage (..))
import TH (makeInstance)

class HomeMsg msg where
  handle ::
    Env -> Device -> msg -> ExceptT ErrorStack IO ()

data SomeHomeMsg = forall msg. HomeMsg msg => SomeHomeMsg msg

-- $( makeInstance
--       ''HomeMsg
--       ''Env
--       ''Proto.HomeEnvelope
--       'Proto.maybe'payload
--       ''Proto.HomeEnvelope'Payload
--  )

-- data ExHomeHandler = forall a. HomeHandler a => ExHomeHandler a

-- instance HomeHandler ExHomeHandler where
--   homeHandler island (ExHomeHandler msg) = homeHandler island msg

-- -- Make HomeHandler Home.Envelope instance.

-- $( makeInstance
--     ''HomeHandler
--     ''Proto.HomeEnvelope
--     'Proto.maybe'payload
--     ''Proto.HomeEnvelope'Payload
--  )

-- -- * Non protobuf messages

-- data EstablishTLSConnection = EstablishTLSConnection
--   { clientParams :: {-# UNPACK #-} !TLS.ClientParams
--   , hostname :: {-# UNPACK #-} !T.Text
--   , port :: {-# UNPACK #-} !T.Text
--   }

-- instance HomeHandler EstablishTLSConnection where
--   homeHandler _device msg@(EstablishTLSConnection params host port) = do
--     env <- getEnv
--     loop <- getLoop
--     let registry = env ^. router . connectionsRegistry

--     void . liftIO $
--       bracketOnError
--         (aquireClientSocket (T.unpack host) (T.unpack port))
--         (traverse_ $ \(sock, _) -> close sock)
--         (withSocket env loop registry)
--    where
--     withSocket _ loop _ Nothing = do
--       liftIO $ putStrLn "Failed to reach server"
--       void $ setTimeoutIO (Home, ExHomeHandler msg) 2000 loop
--     withSocket env loop registry (Just (sock, addr)) = do
--       putStrLn $ "Connecting to host: " <> show (addrAddress addr)
--       success <- connectToHost sock addr
--       if success
--         then do
--           putStrLn $ "Connected to " <> show (addrAddress addr)
--           mConn <- upgradeSocket params sock
--           case mConn of
--             Nothing -> putStrLn "Failed to upgrade socket to TLS context"
--             Just conn -> do
--               putStrLn "Established TLS connection"
--               threadId <- forkIO . void $ runConnection loop (env ^. router) conn
--               addConnection Proxy threadId conn registry
--         else do
--           putStrLn $
--             "Failed to connect to server: "
--               <> show (addrAddress addr)
--               <> ". Trying again in 2s..."
--           void $ setTimeoutIO (Home, ExHomeHandler msg) 2000 loop

--     runConnection loop rtr conn = do
--       merror <- recvAndDispatch conn
--       case merror of
--         Left (TLSRxError err) -> do
--           print $ "ERROR: " <> show err
--           cleanup conn
--           void $ setTimeoutIO (Home, ExHomeHandler msg) 2000 loop
--         Right bytes -> do
--           handleBytes @Proto.WrappedEnvelope bytes rtr $ \src wrappedEnv ->
--             case wrappedEnv ^. Proto.maybe'wrappedPayload of
--               Just (Proto.WrappedEnvelope'HomeMsg homeMsg) -> addMsgIO (src, ExHomeHandler homeMsg) loop
--               _ -> putStrLn "Dropping message for wrong device..."
--           runConnection loop rtr conn

-- -- * Helper functions

-- notifyProxyRadioStatus ::
--   Router -> Device -> StreamStatus -> StationId -> IO Bool
-- notifyProxyRadioStatus rtr island status stationId =
--   trySendMessage
--     rtr
--     island
--     ( wrapProxyMsg $
--         defMessage @Proto.RadioStatusUpdate
--           & Proto.status
--           .~ toMessage status
--           & Proto.currentStationId
--           .~ stationId
--     )

-- -- * Protobuf handlers

-- -- | Try to make a new asynchronous audio stream in a separate
-- -- thread. If one exists, report the error.
-- instance HomeHandler Proto.ModifyRadioRequest where
--   homeHandler src req = do
--     env <- getEnv
--     let notify = do
--           (_, status, stationId) <- atomicModifyIORef' (env ^. audioStreamRef) $ \a -> (a, a)
--           void $ notifyProxyRadioStatus (env ^. router) src status stationId
--     (mThreadId, _, _) <- liftIO . atomicModifyIORef' (env ^. audioStreamRef) $ \a -> (a, a)
--     case (mThreadId, req ^. Proto.maybe'station) of
--       (Just _, Just _) -> do
--         liftIO $ reportLog (env ^. router) Info "Audio stream already exists"
--       (Just threadId, Nothing) -> do
--         liftIO $ killThread threadId
--       (Nothing, Nothing) -> liftIO $ reportLog (env ^. router) Info "Radio not playing"
--       (Nothing, Just station) -> do
--         let updateStreamStatus status = do
--               atomicModifyIORef' (env ^. audioStreamRef) $ \st -> (st, ()) & _1 . _2 .~ status
--               notify
--         threadId <-
--           liftIO $
--             forkFinally
--               (startAudioStream (env ^. router) (station ^. Proto.url) updateStreamStatus)
--               ( \_ -> do
--                   atomicModifyIORef' (env ^. audioStreamRef) $ \st ->
--                     (st, ())
--                       & _1
--                       . _1
--                       .~ Nothing
--                       & _1
--                       . _2
--                       .~ Off
--                   void notify
--                   reportLog (env ^. router) Info "Stopping radio stream"
--               )
--         liftIO
--           . atomicModifyIORef'
--             (env ^. audioStreamRef)
--           $ \st ->
--             (st, ())
--               & _1
--               . _1
--               ?~ threadId
--               & _1
--               . _3
--               .~ (station ^. Proto.id)
--     void . liftIO $ notify

-- -- | Spawn a new thread and try to connect to the given TCP server.

-- --
-- instance HomeHandler Proto.DeviceData where
--   homeHandler _ msg = do
--     env <- getEnv
--     liftIO . forM_ (filter (/= Home) devices) $ \island ->
--       trySendMessage (env ^. router) island msg

-- instance HomeHandler Proto.MemoryInformation where
--   homeHandler src msg = do
--     env <- getEnv
--     forM_ proxies $ \proxy -> do
--       liftIO $ do
--         void . tryForwardMessage (env ^. router) src proxy $ wrapProxyMsg msg

-- instance HomeHandler Proto.CheckMemoryUsage where
--   homeHandler _ _ = do
--     env <- getEnv
--     mMemInfo <- liftIO getMemoryInformation
--     case mMemInfo of
--       Nothing -> liftIO $ reportLog (env ^. router) Error "Failed to get memory info"
--       Just memInfo -> do
--         addMsg (Home, ExHomeHandler $ toMessage @Proto.MemoryInformation memInfo)
