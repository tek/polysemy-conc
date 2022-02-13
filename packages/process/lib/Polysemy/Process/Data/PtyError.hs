module Polysemy.Process.Data.PtyError where

data PtyError =
  CreationFailed
  deriving stock (Eq, Show)
