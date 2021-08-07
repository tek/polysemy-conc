{-# options_haddock prune #-}
-- |Description: Scoped Effect, Internal
module Polysemy.Conc.Effect.Scoped where

import Polysemy (transform)
import Polysemy.Internal (Sem (Sem, runSem), liftSem, send)
import Polysemy.Internal.Union (Weaving (Weaving), decomp, hoist, injWeaving)

-- |@Scoped@ transforms a program so that @effect@ is associated with a @resource@ within that program.
-- This requires the interpreter for @effect@ to be parameterized by @resource@ and constructed for every program using
-- @Scoped@ separately.
--
-- An application for this is 'Polysemy.Conc.Events', in which each program using the effect 'Consume' is interpreted
-- with its own copy of the event channel; or a database transaction, in which a transaction handle is created for the
-- wrapped program and passed to the interpreter for the database effect.
--
-- Resource creation is performed by the function passed to 'runScoped'.
--
-- The constructors are not intended to be used directly; the smart constructor 'scoped' is used like a local
-- interpreter for @effect@.
data Scoped (resource :: Type) (effect :: Effect) :: Effect where
  Run :: ∀ resource effect m a . resource -> effect m a -> Scoped resource effect m a
  InScope :: ∀ resource effect m a . (resource -> m a) -> Scoped resource effect m a

interpretH' ::
  ∀ e r .
  (∀ x . Weaving e (Sem (e : r)) x -> Sem r x) ->
  InterpreterFor e r
interpretH' h sem =
  Sem \ k -> runSem sem $ decomp >>> \case
    Right wav -> runSem (h wav) k
    Left g -> k $ hoist (interpretH' h) g

-- |Constructor for 'Scoped', taking a nested program and transforming all instances of @effect@ to
-- @Scoped resource effect@.
scoped ::
  ∀ resource effect r .
  Member (Scoped resource effect) r =>
  InterpreterFor effect r
scoped main =
  send $ InScope @resource @effect \ resource ->
    transform @effect (Run resource) main

-- |Interpreter for 'Scoped', taking a @resource@ allocation function and a parameterized interpreter for the plain
-- @effect@.
--
-- @scope@ is a callback function, allowing the user to compute the resource for each program from other effects.
--
-- @scopedInterpreter@ is a regular interpreter that is called with the @resource@ argument produced by @scope@.
-- /Note/: This function will be called for each action in the program, so if the interpreter allocates any resources,
-- they will scoped to a single action. Move them to @scope@ instead.
runScoped ::
  ∀ resource effect r .
  (∀ x . (resource -> Sem r x) -> Sem r x) ->
  (resource -> InterpreterFor effect r) ->
  InterpreterFor (Scoped resource effect) r
runScoped scope scopedInterpreter =
  run
  where
    run :: InterpreterFor (Scoped resource effect) r
    run =
      interpretH' \ (Weaving effect s wv ex ins) -> case effect of
        Run resource act ->
          scopedInterpreter resource (liftSem $ injWeaving $ Weaving act s (raise . run . wv) ex ins)
        InScope main -> do
          ex <$> scope \ resource -> run (wv (main resource <$ s))

-- |Variant of 'Scoped' in which the resource allocator is a plain action.
runScopedAs ::
  ∀ resource effect r .
  Sem r resource ->
  (resource -> InterpreterFor effect r) ->
  InterpreterFor (Scoped resource effect) r
runScopedAs resource =
  runScoped \ f -> f =<< resource
