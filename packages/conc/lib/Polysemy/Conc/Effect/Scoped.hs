{-# options_haddock prune #-}

-- |Description: Scoped Effect, Internal
module Polysemy.Conc.Effect.Scoped where

import Polysemy.Conc.Effect.PScoped (PScoped (InScope, Run))

-- |@Scoped@ transforms a program so that @effect@ is associated with a @resource@ within that program.
-- This requires the interpreter for @effect@ to be parameterized by @resource@ and constructed for every program using
-- @Scoped@ separately.
--
-- An application for this is 'Polysemy.Conc.Events', in which each program using the effect 'Polysemy.Conc.Consume' is
-- interpreted with its own copy of the event channel; or a database transaction, in which a transaction handle is
-- created for the wrapped program and passed to the interpreter for the database effect.
--
-- Resource creation is performed by the function passed to 'Polysemy.Conc.Interpreter.interpretScoped' and its
-- variants.
--
-- The constructors are not intended to be used directly; the smart constructor 'scoped' is used like a local
-- interpreter for @effect@.
type Scoped resource effect =
  PScoped () resource effect

-- |Constructor for 'Scoped', taking a nested program and transforming all instances of @effect@ to
-- @Scoped resource effect@.
scoped ::
  ∀ resource effect r .
  Member (Scoped resource effect) r =>
  InterpreterFor effect r
scoped main =
  send $ InScope @() @resource @effect () \ resource ->
    transform @effect (Run @() resource) main
