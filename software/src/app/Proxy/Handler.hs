{-# LANGUAGE TemplateHaskell #-}

module Proxy.Handler (
    ProxyHandler (..),
    ExProxyHandler (..),
) where

import ConnectionManager (Island (..))
import Control.Concurrent (modifyMVar_)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ask)
import Data.Map.Strict qualified as Map
import EventLoop (EventLoop)
import Home.AudioStream (
    StreamStatus (..),
    protoRadioStatusResponseToStreamStatus,
 )
import Lens.Micro ((^.))
import Proto.Messages qualified as Proto
import Proto.Messages_Fields qualified as Proto
import ProtoHelper (FromMessage (..))
import Proxy.Env (EnvT, streamStatusState, systemMap)
import State (fulfilPromise)
import TH (makeInstance)

class ProxyHandler msg where
    proxyHandler ::
        EventLoop EnvT (Island, ExProxyHandler) -> Island -> msg -> EnvT ()

data ExProxyHandler = forall a. (ProxyHandler a) => ExProxyHandler a

instance ProxyHandler ExProxyHandler where
    proxyHandler loop island (ExProxyHandler msg) = proxyHandler loop island msg

-- * Message instances

-- Make ProxyHandler Proto.ProxyRecieveEnvelope instance.
$( makeInstance
    ''ProxyHandler
    ''Proto.ProxyEnvelope
    'Proto.maybe'payload
    ''Proto.ProxyEnvelope'Payload
 )

-- | Received acknowledgement from Home for ModifyRadioRequest
instance ProxyHandler Proto.ModifyRadioResponse where
    proxyHandler _ _ resp = do
        env <- ask
        liftIO $
            fulfilPromise
                (StreamStatus (fromMessage <$> resp ^. Proto.maybe'newStream))
                (env ^. streamStatusState)

{- | Received acknowledgement from Home for ModifyRadioRequest. Update
promise so HTTP Handler can return.
-}
instance ProxyHandler Proto.GetRadioStatusResponse where
    proxyHandler _ _ resp = do
        env <- ask
        liftIO $
            fulfilPromise
                (protoRadioStatusResponseToStreamStatus resp)
                (env ^. streamStatusState)

instance ProxyHandler Proto.IslandSystemData where
    proxyHandler _ _ resp = do
        env <- ask
        liftIO $ modifyMVar_ (env ^. systemMap) $ \sysMap ->
            let island = fromMessage $ resp ^. Proto.island
                sysData = fromMessage $ resp ^. Proto.systemData
             in pure $ Map.insert island sysData sysMap
