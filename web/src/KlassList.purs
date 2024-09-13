module KlassList (klassList, klassList_) where

import Prelude

import Data.Tuple (Tuple, fst, snd)
import Deku.Attribute (Attribute)
import FRP.Poll (Poll)
import Deku.DOM.Attributes as DA
import Data.Array as Array

klassList :: forall r. Array (Tuple (Poll String) Boolean) -> Poll (Attribute (klass :: String | r))
klassList arr =
  DA.klass $
    Array.foldl (\acc cur -> (\a b -> a <> " " <> b) <$> acc <*> cur) (pure "") (fst <$> Array.filter snd arr)

klassList_ :: forall r. Array (Tuple String Boolean) -> Poll (Attribute (klass :: String | r))
klassList_ arr =
  DA.klass_ $
    Array.foldl (\acc cur -> acc <> " " <> cur) "" (fst <$> Array.filter snd arr)
