module Home.Main where

import Control.Monad.Reader
import EventLoop
import Home.Env
import Home.Handler

main :: IO ()
main = do
    env <- mkEnv
    runReaderT action env
  where
    action = do
        evLoop <- mkEventLoop @(ExHomeHandler Env) homeHandler
        addMsg evLoop $ ExHomeHandler Foo1
        addMsg evLoop $ ExHomeHandler Foo1
        addMsg evLoop $ ExHomeHandler Foo1
        addMsg evLoop $ ExHomeHandler Foo1
        addMsg evLoop $ ExHomeHandler Foo1
        addMsg evLoop $ ExHomeHandler Foo1
        addMsg evLoop $ ExHomeHandler Foo1
        addMsg evLoop $ ExHomeHandler Foo1
        addMsg evLoop $ ExHomeHandler Foo1
        addMsg evLoop $ ExHomeHandler Foo1
        addMsg evLoop $ ExHomeHandler Foo1
