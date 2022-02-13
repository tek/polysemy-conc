{-# options_haddock prune #-}

-- |Description: Scoped Interpreters, Internal
module Polysemy.Conc.Interpreter.Scoped where

import Polysemy (Tactical, insertAt)
import Polysemy.Internal (Sem (Sem, runSem), liftSem)
import Polysemy.Internal.Index (InsertAtIndex)
import Polysemy.Internal.Tactics (liftT, runTactics)
import Polysemy.Internal.Union (Weaving (Weaving), decomp, hoist, injWeaving)
import Polysemy.Resume (Stop, runStop, type (!!))
import Polysemy.Resume.Data.Resumable (Resumable (Resumable))

import Polysemy.Conc.Effect.Scoped (Scoped (InScope, Run))

interpretH' ::
  ∀ e r .
  (∀ x . Weaving e (Sem (e : r)) x -> Sem r x) ->
  InterpreterFor e r
interpretH' h (Sem m) =
  Sem \ k -> m $ decomp >>> \case
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

-- |Variant of 'runScoped' in which the resource allocator is a plain action.
runScopedAs ::
  ∀ resource effect r .
  Sem r resource ->
  (resource -> InterpreterFor effect r) ->
  InterpreterFor (Scoped resource effect) r
runScopedAs resource =
  runScoped \ f -> f =<< resource

-- |Variant of 'runScoped' that takes a higher-order handler instead of an interpreter.
interpretScopedH ::
  ∀ resource effect r .
  (∀ x . (resource -> Sem r x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) r x) ->
  InterpreterFor (Scoped resource effect) r
interpretScopedH withResource scopedHandler =
  run
  where
    run :: InterpreterFor (Scoped resource effect) r
    run =
      interpretH' \ (Weaving effect s wv ex ins) -> case effect of
        Run resource act ->
          ex <$> runTactics s (raise . run . wv) ins (run . wv) (scopedHandler resource act)
        InScope main ->
          ex <$> withResource \ resource -> run (wv (main resource <$ s))

-- |Variant of 'runScoped' that takes a handler instead of an interpreter.
interpretScoped ::
  ∀ resource effect r .
  (∀ x . (resource -> Sem r x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem r x) ->
  InterpreterFor (Scoped resource effect) r
interpretScoped withResource scopedHandler =
  interpretScopedH withResource \ r e -> liftT (scopedHandler r e)

-- |Variant of 'interpretScoped' in which the resource allocator is a plain action.
interpretScopedAs ::
  ∀ resource effect r .
  Sem r resource ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem r x) ->
  InterpreterFor (Scoped resource effect) r
interpretScopedAs resource =
  interpretScoped \ f -> f =<< resource

-- |Combined higher-order interpreter for 'Scoped' and 'Resumable'.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it.
interpretScopedResumableH ::
  ∀ resource effect err r .
  (∀ x . (resource -> Sem (Stop err : r) x) -> Sem (Stop err : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) (Stop err : r) x) ->
  InterpreterFor (Scoped resource effect !! err) r
interpretScopedResumableH withResource scopedHandler =
  run
  where
    run :: InterpreterFor (Scoped resource effect !! err) r
    run =
      interpretH' \ (Weaving (Resumable inner) s' dist' ex' ins') ->
        case inner of
          Weaving effect s dist ex ins -> do
            let
              handleScoped = \case
                Run resource act ->
                  scopedHandler resource act
                InScope main ->
                  raise (withResource \ resource -> Compose <$> raise (run (dist' (dist (main resource <$ s) <$ s'))))
              tac =
                runTactics
                (Compose (s <$ s'))
                (raise . raise . run . fmap Compose . dist' . fmap dist . getCompose)
                (ins <=< ins' . getCompose)
                (raise . run . fmap Compose . dist' . fmap dist . getCompose)
                (handleScoped effect)
              exFinal = ex' . \case
                Right (Compose a) -> Right . ex <$> a
                Left err -> Left err <$ s'
            exFinal <$> runStop tac

-- |Combined interpreter for 'Scoped' and 'Resumable'.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it.
interpretScopedResumable ::
  ∀ resource effect err r .
  (∀ x . (resource -> Sem (Stop err : r) x) -> Sem (Stop err : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : r) x) ->
  InterpreterFor (Scoped resource effect !! err) r
interpretScopedResumable withResource scopedHandler =
  interpretScopedResumableH withResource \ r e -> liftT (scopedHandler r e)
