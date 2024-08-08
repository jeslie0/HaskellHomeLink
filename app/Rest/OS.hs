{-# LANGUAGE DataKinds #-}
module Rest.OS where

import Data.Text qualified as T
import System.OS (getOSInfo, OSInfo)
import Control.Monad.IO.Class (liftIO)
import Error (Error(..))
import TinyServant.Server (Server)
import TinyServant.API (StdMethod(..))
import TinyServant.Combinators ((:>))
import TinyServant.API.UVerb (Verb, WithStatus (..), UVerb)
import TinyServant.Server.UVerb (respond)
import TinyServant.API.ContentTypes (JSON)

type OSAPI =
  "OS" :> UVerb GET '[JSON] '[WithStatus 200 OSInfo, WithStatus 201 T.Text]

osServer :: Server OSAPI
osServer = do
  res <- handlePrettyName
  case res of
    Left txt -> respond @(WithStatus 201 T.Text) $ WithStatus txt
    Right j -> respond @(WithStatus 200 OSInfo) $ WithStatus j

handlePrettyName :: IO (Either T.Text OSInfo)
handlePrettyName = do
  parserResult <- liftIO getOSInfo
  return $ case parserResult of
    Left err ->  Left . errorString $ err
    Right info -> Right info
