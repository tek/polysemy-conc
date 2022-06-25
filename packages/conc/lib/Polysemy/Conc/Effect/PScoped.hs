{-# options_haddock prune #-}

-- |Description: PScoped Effect, Internal
module Polysemy.Conc.Effect.PScoped where

-- |@PScoped@ transforms a program so that @effect@ is associated with a @resource@ within that program.
-- This requires the interpreter for @effect@ to be parameterized by @resource@ and constructed for every program using
-- @PScoped@ separately.
--
-- An application for this is 'Polysemy.Conc.Events', in which each program using the effect 'Polysemy.Conc.Consume' is
-- interpreted with its own copy of the event channel; or a database transaction, in which a transaction handle is
-- created for the wrapped program and passed to the interpreter for the database effect.
--
-- Resource creation is performed by the function passed to 'Polysemy.Conc.Interpreter.interpretPScoped' and its
-- variants.
--
-- The constructors are not intended to be used directly; the smart constructor 'pscoped' is used like a local
-- interpreter for @effect@.
-- 'pscoped' takes an argument of type @param@, which will be passed through to the interpreter, to be used by the
-- resource allocation function.
data PScoped (param :: Type) (resource :: Type) (effect :: Effect) :: Effect where
  Run :: ∀ param resource effect m a . resource -> effect m a -> PScoped param resource effect m a
  InScope :: ∀ param resource effect m a . param -> (resource -> m a) -> PScoped param resource effect m a

-- |Constructor for 'PScoped', taking a nested program and transforming all instances of @effect@ to
-- @PScoped param resource effect@.
-- The value @param@ is passed to the interpreter.
pscoped ::
  ∀ param resource effect r .
  Member (PScoped param resource effect) r =>
  param ->
  InterpreterFor effect r
pscoped param main =
  send $ InScope @param @resource @effect param \ resource ->
    transform @effect (Run @param resource) main
