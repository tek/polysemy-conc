{-# options_haddock prune #-}

-- |Description: Gate effect, Internal
module Polysemy.Conc.Effect.Gate where

import Polysemy.Conc.Effect.Scoped (Scoped, scoped)

-- |A single-use synchronization point that blocks all consumers who called 'gate' until 'signal' is called.
--
-- The constructors are exported from [Polysemy.Conc.Gate]("Polysemy.Conc.Gate").
data Gate :: Effect where
  Signal :: Gate m ()
  Gate :: Gate m ()

makeSem ''Gate

-- |Convenience alias for scoped 'Gate'.
type Gates res =
  Scoped res Gate

-- |Run an action with a locally scoped 'Gate' effect.
--
-- This avoids a dependency on @'Embed' 'IO'@ in application logic while still allowing the effect to be scoped.
withGate ::
  ∀ res r .
  Member (Scoped res Gate) r =>
  InterpreterFor Gate r
withGate =
  scoped @res
