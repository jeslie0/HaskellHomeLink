{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MonoLocalBinds #-}

{-| The Router module defines how messages are sent between
applications. A user of a Router should not be aware of how the
message is being sent (TCP, TLS, UDP, Unix Domain Socket), they just
get told if it has been sent successfully.
-}
module Router where

import Control.Concurrent (MVar, newMVar, readMVar)
import Control.Monad (unless)
import Data.ByteString qualified as B
import Data.Map.Strict qualified as Map
import Island (Island (..))
import Msg (Msg (..))

class BiChannel chan where
    recv :: chan -> Int -> IO Int

    send :: chan -> B.ByteString -> IO ()

    isOpen :: MVar Bool

data ExBiChannel = forall chan. (BiChannel chan) => ExBiChannel chan

data Router = Router
    { _island :: Island
    , _connections :: MVar (Map.Map Island ExBiChannel)
    }

mkHomeRouter :: IO Router
mkHomeRouter = do
    mapMVar <- newMVar Map.empty
    return $
        Router
            { _island = Home
            , _connections = mapMVar
            }

tryConnectToIsland :: Island -> Router -> IO ()
tryConnectToIsland dest (Router _src cons) = do
    unless (dest == _src) $
        case dest of
            Home -> undefined
            Test -> undefined
            Proxy -> do


sendMsg :: forall msg. (Msg msg) => Island -> msg -> Router -> IO ()
sendMsg dest msg (Router _island _connections) = do
    connMap <- readMVar _connections
    case Map.lookup dest connMap of
        Nothing -> pure ()
        Just (ExBiChannel chan) -> send chan $ toBytes msg
