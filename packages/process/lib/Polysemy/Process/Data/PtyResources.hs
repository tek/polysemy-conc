module Polysemy.Process.Data.PtyResources where

import System.Posix (Fd)
import System.Posix.Pty (Pty)

data PtyResources =
  PtyResources {
    primary :: Fd,
    secondary :: Fd,
    handle :: Handle,
    pty :: Pty
  }
