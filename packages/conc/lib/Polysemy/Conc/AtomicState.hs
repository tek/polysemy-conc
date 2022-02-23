-- |Description: AtomicState Interpreters
module Polysemy.Conc.AtomicState where

import Control.Concurrent.STM (newTVarIO)

-- |Convenience wrapper around 'runAtomicStateTVar' that creates a new 'TVar'.
interpretAtomic ::
  ∀ a r .
  Member (Embed IO) r =>
  a ->
  InterpreterFor (AtomicState a) r
interpretAtomic initial sem = do
  tv <- embed (newTVarIO initial)
  runAtomicStateTVar tv sem
{-# inline interpretAtomic #-}
