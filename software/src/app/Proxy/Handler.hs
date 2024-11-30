{-# LANGUAGE TemplateHaskell #-}

module Proxy.Handler (
    ProxyHandler (..),
    ExProxyHandler (..),
    -- ToEnvelope (..),
) where

import ConnectionManager (Island (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ask)
import EventLoop (EventLoop)
import Lens.Micro ((^.))
import Proto.Proxy qualified as Proxy
import Proto.Proxy_Fields qualified as Proxy
import ProtoHelper (protoRadioStatusResponseToStreamStatus)
import Proxy.Env (EnvT, streamStatusState)
import TH (makeInstance)
import State (fulfilPromise)
import Home.AudioStream (StreamStatus(..))

class ProxyHandler msg where
    proxyHandler ::
        EventLoop EnvT (Island, ExProxyHandler) -> Island -> msg -> EnvT ()

data ExProxyHandler = forall a. (ProxyHandler a) => ExProxyHandler a

instance ProxyHandler ExProxyHandler where
    proxyHandler loop island (ExProxyHandler msg) = proxyHandler loop island msg

-- * Message instances

-- Make ProxyHandler Proxy.ProxyRecieveEnvelope instance.
$( makeInstance
    ''ProxyHandler
    ''Proxy.ProxyRecieveEnvelope
    'Proxy.maybe'payload
    ''Proxy.ProxyRecieveEnvelope'Payload
 )

-- | Received acknowledgement from Home for ModifyRadioRequest
instance ProxyHandler Proxy.ModifyRadioResponse where
    proxyHandler _ _ resp = do
        env <- ask
        liftIO $ fulfilPromise (if resp ^. Proxy.mrfRadioOn then Active else Inactive) (env ^. streamStatusState)

-- | Received acknowledgement from Home for ModifyRadioRequest. Update
-- promise so HTTP Handler can return.
instance ProxyHandler Proxy.GetRadioStatusResponse where
    proxyHandler _ _ resp = do
        env <- ask
        liftIO $ fulfilPromise (protoRadioStatusResponseToStreamStatus resp) (env ^. streamStatusState)
