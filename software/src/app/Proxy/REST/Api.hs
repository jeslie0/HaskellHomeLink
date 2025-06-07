{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Proxy.REST.Api (Api, Radio, RadioCommand (..), Connection) where

import Proto.DeviceData qualified as Proto
import Proto.Logging qualified as Proto
import Proto.Radio qualified as Proto
import Servant (
  FromHttpApiData (..),
  Get,
  JSON,
  PostAccepted,
  Put,
  QueryParam,
  Raw,
  ReqBody,
  (:<|>) (..),
  (:>),
 )
import Servant.API.ContentTypes.Proto ( Proto )
import State (StateId)

type Api =
  "api"
    :> "v1"
    :> ( Radio
          :<|> System
          :<|> Memory
          :<|> Logs
       )
    :<|> Raw

type Radio =
  "radio"
    :> ( Get '[Proto] Proto.GetRadioStatusResponse
          :<|> "modify"
            :> ReqBody '[Proto] Proto.ModifyRadioRequest
            :> QueryParam "stateId" StateId
            :> PostAccepted '[JSON] Bool
       )

type Memory = "memory" :> Get '[Proto] Proto.AllDeviceMemoryData

type System =
  "system" :> Get '[Proto] Proto.AllDeviceData

type Logs =
  "logs" :> Get '[Proto] Proto.Logs

data RadioCommand = Start | Stop

instance FromHttpApiData RadioCommand where
  parseUrlPiece "start" = Right Start
  parseUrlPiece "stop" = Right Stop
  parseUrlPiece other = Left $ "Incorrect path piece: " <> other

type Connection = "connection" :> Put '[JSON] Bool
