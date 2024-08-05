{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module TinyServant where

import Control.Applicative
import Data.ByteString.Lazy.Char8 qualified as BC
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import GHC.TypeLits
import Network.HTTP.Types (status200)
import Network.Wai (Application, Request (..), Response, responseLBS)
import Text.Read (readMaybe)

type MyAPI = "date" :> Get Int
        :<|> "time" :> Capture Int :> Get Int

handleDate :: IO Int
handleDate = return 10

handleTime :: Int -> IO Int
handleTime n = return $ n * 2

handleMyAPI :: Server MyAPI
handleMyAPI = handleDate :<|> handleTime

-- * Basic API types

-- We can include content type information here?
data Get (a :: Type)

data Capture (a :: Type)

-- * Combinators

data (a :: k) :> (b :: Type)

infixr 9 :>

data a :<|> b = a :<|> b

infixr 8 :<|>

-- * Server type family

type family Server layout :: Type

-- By using IO here, we have our server living in the IO monad, as
-- opposed to the Handler monad that Servant defines
type instance Server (Get a) = IO a

type instance Server (a :<|> b) = Server a :<|> Server b

type instance Server ((s :: Symbol) :> r) = Server r

type instance Server (Capture a :> r) = a -> Server r

-- * HasServer Class

-- The route function's purpose is to take an incomping request and
-- dispatch it to the correct handler.

class HasServer layout where
  route :: Proxy layout -> Server layout -> [T.Text] -> Maybe (IO Response)

instance (Show a) => HasServer (Get a) where
  route :: Proxy (Get a) -> IO a -> [T.Text] -> Maybe (IO Response)
  route _ handler _ = Just $ responseLBS status200 [] . BC.pack . show <$> handler
  route _ _ _ = Nothing

instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  route ::
    Proxy (a :<|> b) ->
    (Server a :<|> Server b) ->
    [T.Text] ->
    Maybe (IO Response)
  route _ (handlera :<|> handlerb) xs =
    route (Proxy :: Proxy a) handlera xs
      <|> route (Proxy :: Proxy b) handlerb xs

instance (KnownSymbol s, HasServer r) => HasServer ((s :: Symbol) :> r) where
  route ::
    Proxy (s :> r) ->
    Server r ->
    [T.Text] ->
    Maybe (IO Response)
  route _ handler (x : xs) =
    if symbolVal (Proxy :: Proxy s) == T.unpack x then route (Proxy :: Proxy r) handler xs else Nothing
  route _ _ _ = Nothing

instance (Read a, HasServer r) => HasServer (Capture a :> r) where
  route ::
    Proxy (Capture a :> r) ->
    (a -> Server r) ->
    [T.Text] ->
    Maybe (IO Response)
  route _ handler (x : xs) = do
    a <- readMaybe .T.unpack $ x
    route (Proxy @r) (handler a) xs
  route _ _ _ = Nothing


-- * Server interpretation

-- | Serve function. This gives the implementation of our API as a
-- server, routing requests to the correct handlers.
serve :: (HasServer layout) => Proxy layout -> Server layout -> Application
serve proxLayout server request respHandler =
  case route proxLayout server (pathInfo request) of
    Nothing -> ioError (userError "404")
    Just m -> m >>= respHandler

-- WAI application

