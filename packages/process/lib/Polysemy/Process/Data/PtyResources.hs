{-# options_haddock prune #-}

-- |Description: PtyResources ADT, Internal
module Polysemy.Process.Data.PtyResources where

import System.IO (Handle)
import System.Posix (Fd)
import System.Posix.Pty (Pty)

-- |The resources used by the default interpreter for 'Polysemy.Process.Pty'.
data PtyResources =
  PtyResources {
    primary :: Fd,
    secondary :: Fd,
    handle :: Handle,
    pty :: Pty
  }
