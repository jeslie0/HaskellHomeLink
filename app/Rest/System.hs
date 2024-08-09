{-# LANGUAGE DataKinds #-}

module Rest.System where

import Control.Monad.IO.Class (liftIO)
import Error (ErrorStack, toEitherErrorStack)
import Rest.Utils (eitherErrorStackToServer)
import System.OS (OSInfo, getOSInfo)
import TinyServant.API (StdMethod (..))
import TinyServant.API.ContentTypes (JSON)
import TinyServant.API.UVerb (UVerb, WithStatus (..))
import TinyServant.Combinators ((:>), (:<|>) (..))
import TinyServant.Server (Server)
import System.CPU (CPUData, getCPUData)

type SystemAPI =
  "system" :> ("os" :> UVerb GET '[JSON] '[WithStatus 200 OSInfo, WithStatus 500 ErrorStack]
              :<|> "cpu" :> UVerb GET '[JSON] '[WithStatus 200 CPUData, WithStatus 500 ErrorStack]
              )

osServer :: Server SystemAPI
osServer =
  (handleOSInfo >>= eitherErrorStackToServer)
  :<|> (getCPUData >>= eitherErrorStackToServer)

handleOSInfo :: IO (Either ErrorStack OSInfo)
handleOSInfo = do
  parserResult <- liftIO getOSInfo
  return $ toEitherErrorStack parserResult

handleCPUData :: IO (Either ErrorStack CPUData)
handleCPUData = getCPUData
