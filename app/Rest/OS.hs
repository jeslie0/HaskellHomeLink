{-# LANGUAGE DataKinds #-}
module Rest.OS where

import Data.Text qualified as T
import System.OS (getOSInfo, OSInfo)
import Control.Monad.IO.Class (liftIO)
import Error (Error(..))
import TinyServant

type OSAPI =
  "OS" :> Get (Either T.Text OSInfo)

osServer :: Server OSAPI
osServer = handlePrettyName

handlePrettyName :: IO (Either T.Text OSInfo)
handlePrettyName = do
  parserResult <- liftIO getOSInfo
  case parserResult of
    Left err -> return . Left . errorString $ err
    Right info -> return . Right $ info
