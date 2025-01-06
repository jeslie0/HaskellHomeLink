{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module REST.Api (Api, Radio, RadioCommand (..), Connection) where

import Proto.Messages qualified as Proto
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
import Servant.API.ContentTypes.Proto
import State (StateId)

type Api =
  "api" :> "v1" :> (Radio :<|> System :<|> Memory)
    :<|> Raw

type Radio =
  "radio"
    :> ( Get '[Proto] Proto.GetRadioStatusResponse
          :<|> "modify"
            :> ReqBody '[Proto] Proto.ModifyRadioRequest
            :> QueryParam "stateId" StateId
            :> PostAccepted '[JSON] Bool
       )

type Memory = "memory" :> Get '[Proto] Proto.AllIslandMemoryData

type System =
  "system" :> Get '[Proto] Proto.IslandsSystemData

data RadioCommand = Start | Stop

instance FromHttpApiData RadioCommand where
  parseUrlPiece "start" = Right Start
  parseUrlPiece "stop" = Right Stop
  parseUrlPiece other = Left $ "Incorrect path piece: " <> other

type Connection = "connection" :> Put '[JSON] Bool
