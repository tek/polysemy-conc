module Polysemy.Process.Effect.Pty where

import Polysemy.Conc.Effect.Scoped (Scoped, scoped)

newtype Rows =
  Rows { unRows :: Int }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord)

newtype Cols =
  Cols { unCols :: Int }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord)

data Pty :: Effect where
  Handle :: Pty m Handle
  Resize :: Rows -> Cols -> Pty m ()

makeSem ''Pty

withPty ::
  ∀ resource r .
  Member (Scoped resource Pty) r =>
  InterpreterFor Pty r
withPty =
  scoped @resource
