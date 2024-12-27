module Island (Island (..), islands) where

data Island
    = Home
    | Proxy
    | Test
    deriving (Eq, Show, Ord)

islands :: [Island]
islands =
  [Home, Proxy, Test]
