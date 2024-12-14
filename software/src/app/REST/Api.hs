{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module REST.Api (Api, Radio, RadioCommand (..), Connection) where

import Proto.Proxy qualified as Proxy
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
    "api" :> "v1" :> (Radio)
        :<|> Raw

type Radio =
    "radio"
        :> ( Get '[Proto] Proxy.GetRadioStatusResponse
                :<|> "modify"
                    :> ReqBody '[Proto] Proxy.ModifyRadioRequest
                    :> QueryParam "stateId" StateId
                    :> PostAccepted '[JSON] Bool
           )

data RadioCommand = Start | Stop

instance FromHttpApiData RadioCommand where
    parseUrlPiece "start" = Right Start
    parseUrlPiece "stop" = Right Stop
    parseUrlPiece other = Left $ "Incorrect path piece: " <> other

type Connection = "connection" :> Put '[JSON] Bool
