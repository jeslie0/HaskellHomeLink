module Constants (getApiUrl) where

import Prelude

import Effect (Effect)
import Web.HTML (window)
import Web.HTML.Location (origin)
import Web.HTML.Window (location)

apiExtension :: String
apiExtension = "/api/v1/"

getApiUrl :: Effect String
getApiUrl = do
  win <- window
  loc <- location win
  orgn <- origin loc
  pure $ orgn <> apiExtension
