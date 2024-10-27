module TinyServant.Combinators where

import Data.Kind (Type)

data (a :: k) :> (b :: Type)

infixr 9 :>

data a :<|> b = a :<|> b

infixr 8 :<|>

data Capture (a :: Type)

data Raw
