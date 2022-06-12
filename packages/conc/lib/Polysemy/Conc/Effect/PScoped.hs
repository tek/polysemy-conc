{-# options_haddock prune #-}

-- |Description: PScoped Effect, Internal
module Polysemy.Conc.Effect.PScoped where

-- |@PScoped@ is a variant of 'Polysemy.Conc.Effect.Scoped' that allows the scoped
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
