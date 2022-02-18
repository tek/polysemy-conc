{-# options_haddock prune #-}

-- |Description: Pty Effect, Internal
module Polysemy.Process.Effect.Pty where

import Polysemy.Conc.Effect.Scoped (Scoped, scoped)

-- |Horizontal size of a pseudo terminal in characters.
newtype Rows =
  Rows { unRows :: Int }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord)

-- |Vertical size of a pseudo terminal in characters.
newtype Cols =
  Cols { unCols :: Int }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord)

-- |A pseudo terminal, to be scoped with 'withPty'.
data Pty :: Effect where
  -- |The file descriptor that can be connected to stdio of a process.
  Handle :: Pty m Handle
  -- |Set the size of the terminal.
  Resize :: Rows -> Cols -> Pty m ()
  -- |Get the size of the terminal.
  Size :: Pty m (Rows, Cols)

makeSem ''Pty

-- |Bracket an action with the creation and destruction of a pseudo terminal.
withPty ::
  ∀ resource r .
  Member (Scoped resource Pty) r =>
  InterpreterFor Pty r
withPty =
  scoped @resource
