module State (
    State,
    StateId,
    mkState,
    waitForStateUpdate,
    fulfilPromise,
    withState,
    setState,
) where

import Control.Concurrent (
    MVar,
    modifyMVar_,
    newMVar,
    putMVar,
    readMVar,
    takeMVar,
    withMVar, tryPutMVar,
 )
import Control.Monad (void, unless)
import Data.Int (Int32)
import System.Random (StdGen, genWord32, initStdGen, random)

type StateId = Int32

newtype State a = State (MVar StdGen, MVar a)

-- | Create a new state from the given value
mkState :: a -> IO (State a)
mkState a = do
    gen <- initStdGen
    mvarGen <- newMVar gen
    mvarState <- newMVar a
    pure $ State (mvarGen, mvarState)

{- | Run a given computation on the state, and block until the state
is updated asynchronously. (Think promises and futures in C++). Any
attempt to update the state inside this function will result in a deadlock.
-}
waitForStateUpdate :: State a -> StateId -> (StateId -> a -> IO ()) -> IO ()
waitForStateUpdate (State (mvarGen, mvarState)) stateId action = do
    a <- takeMVar mvarState
    withMVar mvarGen $ \gen -> do
      let currStateId = fst . random $ gen
      if currStateId /= stateId
        then putMVar mvarState a
            -- action currStateId a
        else do
            action currStateId a
    void $ readMVar mvarState

{- | Fill the state. Note - using this inside the waitForStateUpdate
function will cause a deadlock.
-}
fulfilPromise :: a -> State a -> IO ()
fulfilPromise a (State (mvarGen, mvarState)) = do
    modifyMVar_ mvarGen $ \gen -> do
        success <- tryPutMVar mvarState a
        unless success $ modifyMVar_ mvarState $ \_ -> pure a
        pure . snd . genWord32 $ gen

-- | Update the current state. Use inside a waitForStateUpdate call
-- will result in a deadlock.
setState :: a -> State a -> IO ()
setState a (State (mvarGen, mvarState)) = do
    modifyMVar_ mvarGen $ \gen -> do
        modifyMVar_ mvarState $ \_ -> pure a
        pure . snd . genWord32 $ gen

-- | Run an IO action on the current state.
withState :: State a -> (StateId -> a -> IO b) -> IO b
withState (State (mvarGen, mvarState)) action = do
    withMVar mvarState $ \a ->
        withMVar mvarGen $ \gen -> action (fst . random $ gen) a
