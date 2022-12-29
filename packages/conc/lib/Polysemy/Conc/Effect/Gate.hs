{-# options_haddock prune #-}

-- |Description: Gate effect, Internal
module Polysemy.Conc.Effect.Gate where

import Polysemy.Scoped (Scoped_, scoped_)

-- |A single-use synchronization point that blocks all consumers who called 'gate' until 'signal' is called.
--
-- The constructors are exported from [Polysemy.Conc.Gate]("Polysemy.Conc.Gate").
data Gate :: Effect where
  Signal :: Gate m ()
  Gate :: Gate m ()

makeSem ''Gate

-- |Convenience alias for scoped 'Gate'.
type Gates =
  Scoped_ Gate

-- |Run an action with a locally scoped 'Gate' effect.
--
-- This avoids a dependency on @'Embed' 'IO'@ in application logic while still allowing the effect to be scoped.
withGate ::
  Member (Scoped_ Gate) r =>
  InterpreterFor Gate r
withGate =
  scoped_
