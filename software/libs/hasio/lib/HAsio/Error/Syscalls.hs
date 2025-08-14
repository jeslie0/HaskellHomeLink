{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module HAsio.Error.Syscalls (SyscallError (..)) where

import HAsio.Error.Error (Error (..))
import HAsio.Error.ErrorCategory (ErrorCategory (..))

data SyscallError
  = Read
  | Write
  | Open
  | Close
  | Stat
  | EpollCreate
  | EpollCreate1
  | EpollWait
  | EpollCtl
  | EventFd
  | TimerFdCreate
  | TimerFdSetTime
  | TimerFdGetTime
  deriving (Eq, Show, Ord, Enum)

data SystemCategory

instance ErrorCategory SystemCategory where
  getErrorCategoryName = "syscall"

instance Error SyscallError where
  type ECat SyscallError = SystemCategory

  getErrorMessage err =
    case err of
      Read -> "read failed"
      Write -> "write failed"
      Open -> "open failed"
      Close -> "close failed"
      Stat -> "stat failed"
      EpollCreate -> "epoll_create failed"
      EpollCreate1 -> "epoll_create1 failed"
      EpollWait -> "epoll_wait failed"
      EpollCtl -> "epoll_ctl failed"
      EventFd -> "eventfd failed"
      TimerFdCreate -> "timerfd_create failed"
      TimerFdSetTime -> "timerfd_settime failed"
      TimerFdGetTime -> "timergd_gettime failed"
