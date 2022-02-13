{-# options_haddock prune #-}
-- |Description: ProcessError, Internal
module Polysemy.Process.Data.ProcessError where

-- |Signal error for 'Polysemy.Process.Process'.
data ProcessError =
  -- |The process terminated.
  Terminated Text
  deriving stock (Eq, Show)
