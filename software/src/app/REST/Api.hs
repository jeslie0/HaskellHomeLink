{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module REST.Api (Api, Radio, RadioCommand (..), Connection) where

import Servant (
    FromHttpApiData (..),
    Get,
    JSON,
    Put,
    (:<|>) (..),
    (:>), Raw,
 )

type Api =
    "api" :> "v1" :> (Radio :<|> Connection)
        :<|> Raw

type Radio =
    "radio"
        :> ( Get '[JSON] Bool
                :<|> "start" :> Put '[JSON] Bool
                :<|> "stop" :> Put '[JSON] Bool
           )

data RadioCommand = Start | Stop

instance FromHttpApiData RadioCommand where
    parseUrlPiece "start" = Right Start
    parseUrlPiece "stop" = Right Stop
    parseUrlPiece other = Left $ "Incorrect path piece: " <> other

type Connection = "connection" :> Put '[JSON] Bool
