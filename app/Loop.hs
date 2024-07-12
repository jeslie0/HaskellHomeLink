module Loop (forLoop) where

forLoop :: (Monad m) => a -> (a -> Bool) -> (a -> a) -> (a -> m ()) -> m ()
forLoop start cond inc f = go start
  where
    go !x | cond x    = f x >> go (inc x)
          | otherwise = return ()

{-# INLINE forLoop #-}
