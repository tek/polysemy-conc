module Polysemy.Process.Data.ProcessError where

data ProcessError =
  Terminated Text
  deriving (Eq, Show)
