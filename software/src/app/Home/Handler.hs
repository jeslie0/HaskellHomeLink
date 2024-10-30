{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module : Home.Handler
Description : Defines the typeclass and instance for messages being
handled by the Home application.
-}
module Home.Handler (
    HomeHandler (..),
    ExHomeHandler(..),
    Foo1(..)
) where

import Control.Monad.Reader
import Data.IORef.Lifted
import Home.Env (Env (..))

class HomeHandler env a where
    homeHandler :: a -> ReaderT env IO ()

data ExHomeHandler env = forall a. (HomeHandler env a) => ExHomeHandler a

instance HomeHandler env (ExHomeHandler env) where
    homeHandler (ExHomeHandler a) = homeHandler a

data Foo1 = Foo1
instance HomeHandler Env Foo1 where
    homeHandler _ = do
        env <- ask
        !x <- readIORef (_count env)
        liftIO . putStrLn $ "current state = " <> show x
        writeIORef (_count env) (x + 1)
