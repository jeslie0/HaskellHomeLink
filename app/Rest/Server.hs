{-# LANGUAGE DataKinds #-}

module Rest.Server where

import Data.Text qualified as T
import Network.Wai
import Rest.OS (OSAPI, osServer)
import TinyServant
import Data.Typeable (Proxy(..))

type API =
  "api" :> "v1" :> ServiceAPI
    -- :<|> Raw

type ServiceAPI = OSAPI

server :: Server API
server =
  osServer
    -- :<|> serveDirectoryWebApp "."

handleCore :: Int -> IO T.Text
handleCore = return . T.pack . show

app :: Application
app = serve (Proxy @API) server
