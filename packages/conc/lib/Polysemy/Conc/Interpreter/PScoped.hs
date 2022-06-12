{-# options_haddock prune #-}

-- |Description: PScoped Interpreters, Internal
module Polysemy.Conc.Interpreter.PScoped where

import Polysemy.Internal (liftSem)
import Polysemy.Internal.Index (InsertAtIndex)
import Polysemy.Internal.Tactics (liftT, runTactics)
import Polysemy.Internal.Union (Weaving (Weaving), injWeaving)
import Polysemy.Resume (Stop, runStop, type (!!))
import Polysemy.Resume.Effect.Resumable (Resumable (Resumable))

import Polysemy.Conc.Effect.PScoped (PScoped (InScope, Run))
import Polysemy.Conc.Interpreter.Scoped (interpretH')

-- |Interpreter for 'Scoped', taking a @resource@ allocation function and a parameterized interpreter for the plain
-- @effect@.
--
-- @withResource@ is a callback function, allowing the user to acquire the resource for each program from other effects.
--
-- @scopedInterpreter@ is a regular interpreter that is called with the @resource@ argument produced by @scope@.
-- /Note/: This function will be called for each action in the program, so if the interpreter allocates any resources,
-- they will be scoped to a single action. Move them to @withResource@ instead.
runPScoped ::
  ∀ param resource effect r .
  (∀ x . param -> (resource -> Sem r x) -> Sem r x) ->
  (resource -> InterpreterFor effect r) ->
  InterpreterFor (PScoped param resource effect) r
runPScoped withResource scopedInterpreter =
  run
  where
    run :: InterpreterFor (PScoped param resource effect) r
    run =
      interpretH' \ (Weaving effect s wv ex ins) -> case effect of
        Run resource act ->
          scopedInterpreter resource (liftSem $ injWeaving $ Weaving act s (raise . run . wv) ex ins)
        InScope param main ->
          ex <$> withResource param \ resource -> run (wv (main resource <$ s))

-- |Variant of 'runPScoped' in which the resource allocator is a plain action.
runPScopedAs ::
  ∀ param resource effect r .
  (param -> Sem r resource) ->
  (resource -> InterpreterFor effect r) ->
  InterpreterFor (PScoped param resource effect) r
runPScopedAs resource =
  runPScoped \ p f -> f =<< resource p

-- |Variant of 'runPScoped' that takes a higher-order handler instead of an interpreter.
interpretPScopedH ::
  ∀ param resource effect r .
  (∀ x . param -> (resource -> Sem r x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) r x) ->
  InterpreterFor (PScoped param resource effect) r
interpretPScopedH withResource scopedHandler =
  run
  where
    run :: InterpreterFor (PScoped param resource effect) r
    run =
      interpretH' \ (Weaving effect s wv ex ins) -> case effect of
        Run resource act ->
          ex <$> runTactics s (raise . run . wv) ins (run . wv) (scopedHandler resource act)
        InScope param main ->
          ex <$> withResource param \ resource -> run (wv (main resource <$ s))

-- |Variant of 'runPScoped' that takes a handler instead of an interpreter.
interpretPScoped ::
  ∀ param resource effect r .
  (∀ x . param -> (resource -> Sem r x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem r x) ->
  InterpreterFor (PScoped param resource effect) r
interpretPScoped withResource scopedHandler =
  interpretPScopedH withResource \ r e -> liftT (scopedHandler r e)

-- |Variant of 'interpretPScoped' in which the resource allocator is a plain action.
interpretPScopedAs ::
  ∀ param resource effect r .
  (param -> Sem r resource) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem r x) ->
  InterpreterFor (PScoped param resource effect) r
interpretPScopedAs resource =
  interpretPScoped \ p f -> f =<< resource p

-- |Combined higher-order interpreter for 'Scoped' and 'Resumable'.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it.
interpretPScopedResumableH ::
  ∀ resource param effect err r .
  (∀ x . param -> (resource -> Sem (Stop err : r) x) -> Sem (Stop err : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) (Stop err : r) x) ->
  InterpreterFor (PScoped param resource effect !! err) r
interpretPScopedResumableH withResource scopedHandler =
  run
  where
    run :: InterpreterFor (PScoped param resource effect !! err) r
    run =
      interpretH' \ (Weaving (Resumable inner) s' dist' ex' ins') ->
        case inner of
          Weaving effect s dist ex ins -> do
            let
              handleScoped = \case
                Run resource act ->
                  scopedHandler resource act
                InScope param main ->
                  raise (withResource param \ resource -> Compose <$> raise (run (dist' (dist (main resource <$ s) <$ s'))))
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
interpretPScopedResumable ::
  ∀ param resource effect err r .
  (∀ x . param -> (resource -> Sem (Stop err : r) x) -> Sem (Stop err : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : r) x) ->
  InterpreterFor (PScoped param resource effect !! err) r
interpretPScopedResumable withResource scopedHandler =
  interpretPScopedResumableH withResource \ r e -> liftT (scopedHandler r e)

interpretPScopedWithH ::
  ∀ extra param resource effect r r1 .
  r1 ~ (extra ++ r) =>
  InsertAtIndex 1 '[PScoped param resource effect] r1 r (PScoped param resource effect : r1) extra =>
  (∀ x . param -> (resource -> Sem r1 x) -> Sem r x) ->
  (∀ m x . resource -> effect m x -> Tactical effect m r1 x) ->
  InterpreterFor (PScoped param resource effect) r
interpretPScopedWithH withResource scopedHandler =
  interpretH' \case
    Weaving (InScope param main) s wv ex _ ->
      ex <$> withResource param \ resource -> inScope (insertAt @1 @extra (wv (main resource <$ s)))
    _ ->
      error "top level Run"
  where
    inScope :: InterpreterFor (PScoped param resource effect) r1
    inScope =
      interpretH' \case
        Weaving (Run resource act) s wv ex ins ->
          ex <$> runTactics s (raise . inScope . wv) ins (inScope . wv) (scopedHandler resource act)
        _ ->
          error "nested InScope"

interpretPScopedWith ::
  ∀ extra param resource effect r r1 .
  r1 ~ (extra ++ r) =>
  InsertAtIndex 1 '[PScoped param resource effect] r1 r (PScoped param resource effect : r1) extra =>
  (∀ x . param -> (resource -> Sem r1 x) -> Sem r x) ->
  (∀ m x . resource -> effect m x -> Sem r1 x) ->
  InterpreterFor (PScoped param resource effect) r
interpretPScopedWith withResource scopedHandler =
  interpretPScopedWithH @extra withResource \ r e -> liftT (scopedHandler r e)

interpretPScopedWith_ ::
  ∀ extra param effect r r1 .
  r1 ~ (extra ++ r) =>
  InsertAtIndex 1 '[PScoped param () effect] r1 r (PScoped param () effect : r1) extra =>
  (∀ x . param -> Sem r1 x -> Sem r x) ->
  (∀ m x . effect m x -> Sem r1 x) ->
  InterpreterFor (PScoped param () effect) r
interpretPScopedWith_ withResource scopedHandler =
  interpretPScopedWithH @extra (\ p f -> withResource p (f ())) \ () e -> liftT (scopedHandler e)

interpretPScopedResumableWithH ::
  ∀ extra param resource effect err r r1 .
  r1 ~ ((extra ++ '[Stop err]) ++ r) =>
  InsertAtIndex 1 '[PScoped param resource effect !! err] r1 r (PScoped param resource effect !! err : r1) (extra ++ '[Stop err]) =>
  (∀ x . param -> (resource -> Sem r1 x) -> Sem (Stop err : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) r1 x) ->
  InterpreterFor (PScoped param resource effect !! err) r
interpretPScopedResumableWithH withResource scopedHandler =
  run
  where
    run :: InterpreterFor (PScoped param resource effect !! err) r
    run =
      interpretH' \ (Weaving (Resumable inner) s' dist' ex' ins') ->
        case inner of
          Weaving effect s dist ex ins -> do
            let
              handleScoped = \case
                Run _ _ ->
                  error "top level Run"
                InScope param main ->
                  raise (withResource param \ resource -> Compose <$> inScope (insertAt @1 @(extra ++ '[Stop err]) (dist' (dist (main resource <$ s) <$ s'))))
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
    inScope :: InterpreterFor (PScoped param resource effect !! err) r1
    inScope =
      interpretH' \ (Weaving (Resumable inner) s' dist' ex' ins') ->
        case inner of
          Weaving effect s dist ex ins -> do
            let
              handleScoped = \case
                Run resource act ->
                  scopedHandler resource act
                InScope _ _ ->
                  error "nested InScope"
              tac =
                runTactics
                (Compose (s <$ s'))
                (raise . inScope . fmap Compose . dist' . fmap dist . getCompose)
                (ins <=< ins' . getCompose)
                (inScope . fmap Compose . dist' . fmap dist . getCompose)
                (handleScoped effect)
              exFinal = ex' . \case
                Right (Compose a) -> Right . ex <$> a
                Left err -> Left err <$ s'
            exFinal <$> runStop (raise tac)

interpretPScopedResumableWith ::
  ∀ extra param resource effect err r r1 .
  r1 ~ ((extra ++ '[Stop err]) ++ r) =>
  InsertAtIndex 1 '[PScoped param resource effect !! err] r1 r (PScoped param resource effect !! err : r1) (extra ++ '[Stop err]) =>
  (∀ x . param -> (resource -> Sem r1 x) -> Sem (Stop err : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem r1 x) ->
  InterpreterFor (PScoped param resource effect !! err) r
interpretPScopedResumableWith withResource scopedHandler =
  interpretPScopedResumableWithH @extra withResource \ r e -> liftT (scopedHandler r e)

interpretPScopedResumableWith_ ::
  ∀ extra param effect err r r1 .
  r1 ~ ((extra ++ '[Stop err]) ++ r) =>
  InsertAtIndex 1 '[PScoped param () effect !! err] r1 r (PScoped param () effect !! err : r1) (extra ++ '[Stop err]) =>
  (∀ x . param -> Sem r1 x -> Sem (Stop err : r) x) ->
  (∀ r0 x . effect (Sem r0) x -> Sem r1 x) ->
  InterpreterFor (PScoped param () effect !! err) r
interpretPScopedResumableWith_ withResource scopedHandler =
  interpretPScopedResumableWith @extra (\ p f -> withResource p (f ())) (const scopedHandler)
