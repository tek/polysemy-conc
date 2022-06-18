{-# options_haddock prune #-}
-- |Description: ProcessError, Internal
module Polysemy.Process.Data.ProcessError where

import System.Exit (ExitCode)

-- |Signal error for 'Polysemy.Process.Process'.
data ProcessError =
  -- |The process terminated.
  Unknown Text
  |
  Exit ExitCode
  deriving stock (Eq, Show)
