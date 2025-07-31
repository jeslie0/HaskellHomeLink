module System.Epoll.Events where

import System.Epoll.Internal.Events (EpollEvent(..))

import Foreign (Ptr, Storable (..), allocaArray, castPtr, peekArray)

epollWaitSafe :: CInt -> Int -> Int -> IO (Maybe [EpollEvent])
epollWaitSafe epfd maxEvents timeout =
  allocaArray maxEvents $ \eventsPtr -> do
    n <-
      c_epoll_wait_safe epfd eventsPtr (fromIntegral maxEvents) (fromIntegral timeout)
    if n < 0
      then pure Nothing
      else Just <$> peekArray (fromIntegral n) eventsPtr

epollWaitUnSafe :: CInt -> Int -> Int -> IO (Maybe [EpollEvent])
epollWaitUnSafe epfd maxEvents timeout =
  allocaArray maxEvents $ \eventsPtr -> do
    n <-
      c_epoll_wait_safe epfd eventsPtr (fromIntegral maxEvents) (fromIntegral timeout)
    if n < 0
      then pure Nothing
      else Just <$> peekArray (fromIntegral n) eventsPtr
