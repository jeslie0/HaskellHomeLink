{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TinyServant.Server where

import Control.Applicative ((<|>))
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Network.HTTP.Types (Method, methodGet, methodHead)
import Network.Wai (Application, Request (..), Response)
import Text.Read (readMaybe)
import TinyServant.API.UVerb (UVerb)
import TinyServant.Combinators (Capture, (:<|>) (..), (:>), Raw)
import TinyServant.Union (Union)

type family Server layout :: Type

-- By using IO here, we have our server living in the IO monad, as
-- opposed to the Handler monad that Servant defines
type instance Server (UVerb method status as) = IO (Union as)

type instance Server (a :<|> b) = Server a :<|> Server b

type instance Server ((s :: Symbol) :> r) = Server r

type instance Server (Capture a :> r) = a -> Server r

type instance Server (Raw) = IO [T.Text]

class HasServer layout where
  route :: Proxy layout -> Server layout -> [T.Text] -> Request -> Maybe (IO Response)

allowedMethodHead :: Method -> Request -> Bool
allowedMethodHead method request = method == methodGet && requestMethod request == methodHead

instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  route ::
    Proxy (a :<|> b) ->
    (Server a :<|> Server b) ->
    [T.Text] ->
    Request ->
    Maybe (IO Response)
  route _ (handlera :<|> handlerb) paths req =
    route (Proxy @a) handlera paths req
      <|> route (Proxy :: Proxy b) handlerb paths req

instance (KnownSymbol s, HasServer r) => HasServer ((s :: Symbol) :> r) where
  route ::
    Proxy (s :> r) ->
    Server r ->
    [T.Text] ->
    Request ->
    Maybe (IO Response)
  route _ handler (x : xs) req =
    if symbolVal (Proxy :: Proxy s) == T.unpack x then route (Proxy :: Proxy r) handler xs req else Nothing
  route _ _ _ _ = Nothing

instance (Read a, HasServer r) => HasServer (Capture a :> r) where
  route ::
    Proxy (Capture a :> r) ->
    (a -> Server r) ->
    [T.Text] ->
    Request ->
    Maybe (IO Response)
  route _ handler (x : xs) req = do
    a <- readMaybe . T.unpack $ x
    route (Proxy @r) (handler a) xs req
  route _ _ _ _ = Nothing

instance HasServer Raw where
  route ::
    Proxy Raw ->
    IO [T.Text] ->
    [T.Text] ->
    Request ->
    Maybe (IO Response)
  rout _ handler

-- * Server interpretation

-- | Serve function. This gives the implementation of our API as a
-- server, routing requests to the correct handlers.
serve :: (HasServer layout) => Proxy layout -> Server layout -> Application
serve proxLayout server request respHandler =
  case route proxLayout server (pathInfo request) request of
    Just m -> m >>= respHandler
    Nothing -> ioError (userError "404")
