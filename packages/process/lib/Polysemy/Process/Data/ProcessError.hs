{-# options_haddock prune #-}

-- |Description: ProcessError, Internal
module Polysemy.Process.Data.ProcessError where

import System.Exit (ExitCode)

import Polysemy.Process.Data.SystemProcessError (SystemProcessScopeError)

-- |Signal error for 'Polysemy.Process.Process'.
data ProcessError =
  -- |Something broke.
  Unknown Text
  |
  -- |The process failed to start.
  StartFailed SystemProcessScopeError
  |
  -- |The process terminated with exit code.
  Exit ExitCode
  deriving stock (Eq, Show)
