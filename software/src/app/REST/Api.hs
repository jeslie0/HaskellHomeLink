{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module REST.Api (Api, Radio, RadioCommand (..), Connection) where

import Data.Word (Word32)
import Proto.Proxy qualified as Proxy
import Servant (
    FromHttpApiData (..),
    Get,
    JSON,
    NoContent,
    PostAccepted,
    Put,
    PutAccepted,
    QueryParam,
    Raw,
    ReqBody,
    (:<|>) (..),
    (:>),
 )
import Servant.API.ContentTypes.Proto

type Api =
    "api" :> "v1" :> (Radio)
        :<|> Raw

type Radio =
    "radio"
        :> ( Get '[Proto] Proxy.GetRadioStatusResponse
                :<|> "modify"
                    :> ReqBody '[Proto] Proxy.ModifyRadioRequest
                    :> QueryParam "stateId" Word32
                    :> PostAccepted '[JSON] Bool
           )

data RadioCommand = Start | Stop

instance FromHttpApiData RadioCommand where
    parseUrlPiece "start" = Right Start
    parseUrlPiece "stop" = Right Stop
    parseUrlPiece other = Left $ "Incorrect path piece: " <> other

type Connection = "connection" :> Put '[JSON] Bool
