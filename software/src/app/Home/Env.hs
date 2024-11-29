{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MonoLocalBinds #-}

module Home.Env (
    Env,
    EnvT,
    mkEnv,
    audioStreamMVar,
    httpServerMVar,
    router,
    addLocalHTTPServerConnection
) where

import Connection (mkTCPClientConnection)
import ConnectionManager (Island (..), addConnection, getSrcDest)
import Control.Concurrent (MVar, newEmptyMVar, newMVar)
import Control.Monad.Reader (ReaderT)
import Lens.Micro.TH (makeLenses)
import Router (Router, mkRouter, connectionsManager, thisIsland, forwardMsg)
import Threads (AsyncComputation)
import Lens.Micro ((^.))
import qualified Data.ByteString as B
import Msg (fromBytes, Msg)
import Control.Monad (void)

data Env = Env
    { _router :: Router
    , _audioStreamMVar :: MVar (Maybe AsyncComputation)
    , _httpServerMVar :: MVar AsyncComputation
    }

$(makeLenses ''Env)

type EnvT = ReaderT Env IO

mkEnv :: IO Env
mkEnv = do
    _audioStreamMVar <- newMVar Nothing
    _httpServerMVar <- newEmptyMVar
    _router <- mkRouter Home
    pure $
        Env
            { _audioStreamMVar
            , _httpServerMVar
            , _router
            }

addLocalHTTPServerConnection :: forall msg. Msg msg => ((Island, msg) -> IO ()) -> Router -> IO ()
addLocalHTTPServerConnection actOnMsg rtr = do
    connection <-
        mkTCPClientConnection "127.0.0.1" "3000" withBytes
    addConnection LocalProxy connection (rtr ^. connectionsManager)
  where
    withBytes bytes = do
        let (srcDest, payload) = B.splitAt 2 bytes
        case getSrcDest srcDest of
            Nothing -> putStrLn "Could not get source or dest from message"
            Just (src, dest) -> do
                if dest == rtr ^. thisIsland
                    then case fromBytes @msg payload of
                        Left err -> putStrLn err
                        Right envelope -> actOnMsg (src, envelope)
                    else void $ forwardMsg rtr dest bytes
