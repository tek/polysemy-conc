{-# options_haddock prune #-}
-- |Description: Scoped Interpreters, Internal
module Polysemy.Conc.Interpreter.Scoped where

import Polysemy.Internal (Sem (Sem, runSem), liftSem)
import Polysemy.Internal.Union (Weaving (Weaving), decomp, hoist, injWeaving)

import Polysemy.Conc.Effect.Scoped (Scoped (InScope, Run))

interpretH' ::
  ∀ e r .
  (∀ x . Weaving e (Sem (e : r)) x -> Sem r x) ->
  InterpreterFor e r
interpretH' h sem =
  Sem \ k -> runSem sem $ decomp >>> \case
    Right wav -> runSem (h wav) k
    Left g -> k $ hoist (interpretH' h) g


-- |Interpreter for 'Scoped', taking a @resource@ allocation function and a parameterized interpreter for the plain
-- @effect@.
--
-- @withResource@ is a callback function, allowing the user to acquire the resource for each program from other effects.
--
-- @scopedInterpreter@ is a regular interpreter that is called with the @resource@ argument produced by @scope@.
-- /Note/: This function will be called for each action in the program, so if the interpreter allocates any resources,
-- they will be scoped to a single action. Move them to @withResource@ instead.
runScoped ::
  ∀ resource effect r .
  (∀ x . (resource -> Sem r x) -> Sem r x) ->
  (resource -> InterpreterFor effect r) ->
  InterpreterFor (Scoped resource effect) r
runScoped withResource scopedInterpreter =
  run
  where
    run :: InterpreterFor (Scoped resource effect) r
    run =
      interpretH' \ (Weaving effect s wv ex ins) -> case effect of
        Run resource act ->
          scopedInterpreter resource (liftSem $ injWeaving $ Weaving act s (raise . run . wv) ex ins)
        InScope main ->
          ex <$> withResource \ resource -> run (wv (main resource <$ s))

-- |Variant of 'Scoped' in which the resource allocator is a plain action.
runScopedAs ::
  ∀ resource effect r .
  Sem r resource ->
  (resource -> InterpreterFor effect r) ->
  InterpreterFor (Scoped resource effect) r
runScopedAs resource =
  runScoped \ f -> f =<< resource
