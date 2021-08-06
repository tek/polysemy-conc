module Polysemy.Conc.AtomicState where

import Polysemy.AtomicState (runAtomicStateTVar)

-- |Convenience wrapper around 'runAtomicStateTVar' that creates a new 'TVar'.
interpretAtomic ::
  ∀ a r .
  Member (Embed IO) r =>
  a ->
  InterpreterFor (AtomicState a) r
interpretAtomic initial sem = do
  tv <- newTVarIO initial
  runAtomicStateTVar tv sem
{-# inline interpretAtomic #-}
