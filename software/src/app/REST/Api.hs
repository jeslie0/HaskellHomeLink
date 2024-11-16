{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module REST.Api (Api, Radio, RadioCommand(..), Connection, AddMsg) where

import Proto.Home qualified as Home
import Servant (
    Capture,
    FromHttpApiData (..),
    JSON,
    Put,
    (:<|>) (..),
    (:>),
 )

type Api = "api" :> "v1" :> (Radio :<|> Connection)

type Radio = "radio" :> Capture "command" RadioCommand :> Put '[JSON] Bool

data RadioCommand = Start | Stop

instance FromHttpApiData RadioCommand where
    parseUrlPiece "start" = Right Start
    parseUrlPiece "stop" = Right Stop
    parseUrlPiece other = Left $ "Incorrect path piece: " <> other


type Connection = "connection" :> Put '[JSON] Bool

type AddMsg = Home.Envelope -> IO ()
