module Island (Island (..)) where

data Island
    = Home
    | Proxy
    | Test
    deriving (Eq, Show, Ord)
