{-# options_haddock prune #-}

-- |PtyError ADT, Internal
module Polysemy.Process.Data.PtyError where

-- |Internal error used by an interpreter for 'Polysemy.Process.Pty'.
data PtyError =
  CreationFailed
  deriving stock (Eq, Show)
