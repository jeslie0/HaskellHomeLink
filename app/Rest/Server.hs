{-# LANGUAGE DataKinds #-}

module Rest.Server where

import Data.Text qualified as T
import Network.Wai
import Rest.OS (OSAPI, osServer)
import Data.Typeable (Proxy(..))
import TinyServant.Combinators ((:>))
import TinyServant.Server (Server, serve)

type API =
  "api" :> "v1" :> ServiceAPI
    -- :<|> Raw

type ServiceAPI = OSAPI

server :: Server API
server =
  osServer

handleCore :: Int -> IO T.Text
handleCore = return . T.pack . show

app :: Application
app = serve @API (Proxy @API) server