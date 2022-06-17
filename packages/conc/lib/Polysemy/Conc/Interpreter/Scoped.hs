{-# options_haddock prune #-}

-- |Description: Scoped Interpreters, Internal
module Polysemy.Conc.Interpreter.Scoped where

import Polysemy.Internal (Sem (Sem, runSem), liftSem)
import Polysemy.Internal.Index (InsertAtIndex)
import Polysemy.Internal.Tactics (liftT, runTactics)
import Polysemy.Internal.Union (Weaving (Weaving), decomp, hoist, injWeaving)
import Polysemy.Resume (Stop, runStop, type (!!))
import Polysemy.Resume.Effect.Resumable (Resumable (Resumable))

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
  runScoped (=<< resource)

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
  interpretScoped (=<< resource)

-- |Combined higher-order interpreter for 'Scoped' and 'Resumable'.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
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
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
interpretScopedResumable ::
  ∀ resource effect err r .
  (∀ x . (resource -> Sem (Stop err : r) x) -> Sem (Stop err : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : r) x) ->
  InterpreterFor (Scoped resource effect !! err) r
interpretScopedResumable withResource scopedHandler =
  interpretScopedResumableH withResource \ r e -> liftT (scopedHandler r e)

-- |Combined interpreter for 'Scoped' and 'Resumable'.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
-- In this variant, the resource allocator is a plain action.
interpretScopedResumable_ ::
  ∀ resource effect err r .
  Sem (Stop err : r) resource ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : r) x) ->
  InterpreterFor (Scoped resource effect !! err) r
interpretScopedResumable_ resource =
  interpretScopedResumable (=<< resource)

-- |Higher-order interpreter for 'Scoped' that allows the handler to use additional effects that are interpreted by the
-- resource allocator.
interpretScopedWithH ::
  ∀ extra resource effect r r1 .
  r1 ~ (extra ++ r) =>
  InsertAtIndex 1 '[Scoped resource effect] r1 r (Scoped resource effect : r1) extra =>
  (∀ x . (resource -> Sem r1 x) -> Sem r x) ->
  (∀ m x . resource -> effect m x -> Tactical effect m r1 x) ->
  InterpreterFor (Scoped resource effect) r
interpretScopedWithH withResource scopedHandler =
  interpretH' \case
    Weaving (InScope main) s wv ex _ ->
      ex <$> withResource \ resource -> inScope (insertAt @1 @extra (wv (main resource <$ s)))
    _ ->
      error "top level Run"
  where
    inScope :: InterpreterFor (Scoped resource effect) r1
    inScope =
      interpretH' \case
        Weaving (Run resource act) s wv ex ins ->
          ex <$> runTactics s (raise . inScope . wv) ins (inScope . wv) (scopedHandler resource act)
        _ ->
          error "nested InScope"

-- |Interpreter for 'Scoped' that allows the handler to use additional effects that are interpreted by the resource
-- allocator.
interpretScopedWith ::
  ∀ extra resource effect r r1 .
  r1 ~ (extra ++ r) =>
  InsertAtIndex 1 '[Scoped resource effect] r1 r (Scoped resource effect : r1) extra =>
  (∀ x . (resource -> Sem r1 x) -> Sem r x) ->
  (∀ m x . resource -> effect m x -> Sem r1 x) ->
  InterpreterFor (Scoped resource effect) r
interpretScopedWith withResource scopedHandler =
  interpretScopedWithH @extra withResource \ r e -> liftT (scopedHandler r e)

-- |Interpreter for 'Scoped' that allows the handler to use additional effects that are interpreted by the resource
-- allocator.
-- In this variant, no resource is used and the allocator is a plain interpreter.
interpretScopedWith_ ::
  ∀ extra effect r r1 .
  r1 ~ (extra ++ r) =>
  InsertAtIndex 1 '[Scoped () effect] r1 r (Scoped () effect : r1) extra =>
  (∀ x . Sem r1 x -> Sem r x) ->
  (∀ m x . effect m x -> Sem r1 x) ->
  InterpreterFor (Scoped () effect) r
interpretScopedWith_ withResource scopedHandler =
  interpretScopedWithH @extra (\ f -> withResource (f ())) \ () e -> liftT (scopedHandler e)

-- |Combined higher-order interpreter for 'Scoped' and 'Resumable' that allows the handler to use additional effects
-- that are interpreted by the resource allocator.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
interpretScopedResumableWithH ::
  ∀ extra resource effect err r r1 .
  r1 ~ ((extra ++ '[Stop err]) ++ r) =>
  InsertAtIndex 1 '[Scoped resource effect !! err] r1 r (Scoped resource effect !! err : r1) (extra ++ '[Stop err]) =>
  (∀ x . (resource -> Sem r1 x) -> Sem (Stop err : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) r1 x) ->
  InterpreterFor (Scoped resource effect !! err) r
interpretScopedResumableWithH withResource scopedHandler =
  run
  where
    run :: InterpreterFor (Scoped resource effect !! err) r
    run =
      interpretH' \ (Weaving (Resumable inner) s' dist' ex' ins') ->
        case inner of
          Weaving effect s dist ex ins -> do
            let
              handleScoped = \case
                Run _ _ ->
                  error "top level Run"
                InScope main ->
                  raise (withResource \ resource -> Compose <$> inScope (insertAt @1 @(extra ++ '[Stop err]) (dist' (dist (main resource <$ s) <$ s'))))
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
    inScope :: InterpreterFor (Scoped resource effect !! err) r1
    inScope =
      interpretH' \ (Weaving (Resumable inner) s' dist' ex' ins') ->
        case inner of
          Weaving effect s dist ex ins -> do
            let
              handleScoped = \case
                Run resource act ->
                  scopedHandler resource act
                InScope _ ->
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

-- |Combined interpreter for 'Scoped' and 'Resumable' that allows the handler to use additional effects that are
-- interpreted by the resource allocator.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
interpretScopedResumableWith ::
  ∀ extra resource effect err r r1 .
  r1 ~ (extra ++ '[Stop err]) ++ r =>
  InsertAtIndex 1 '[Scoped resource effect !! err] r1 r (Scoped resource effect !! err : r1) (extra ++ '[Stop err]) =>
  (∀ x . (resource -> Sem r1 x) -> Sem (Stop err : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem r1 x) ->
  InterpreterFor (Scoped resource effect !! err) r
interpretScopedResumableWith withResource scopedHandler =
  interpretScopedResumableWithH @extra withResource \ r e -> liftT (scopedHandler r e)

-- |Combined interpreter for 'Scoped' and 'Resumable' that allows the handler to use additional effects that are
-- interpreted by the resource allocator.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
-- In this variant, no resource is used and the allocator is a plain interpreter.
interpretScopedResumableWith_ ::
  ∀ extra effect err r r1 .
  r1 ~ (extra ++ '[Stop err]) ++ r =>
  InsertAtIndex 1 '[Scoped () effect !! err] r1 r (Scoped () effect !! err : r1) (extra ++ '[Stop err]) =>
  (∀ x . Sem r1 x -> Sem (Stop err : r) x) ->
  (∀ r0 x . effect (Sem r0) x -> Sem r1 x) ->
  InterpreterFor (Scoped () effect !! err) r
interpretScopedResumableWith_ extra scopedHandler =
  interpretScopedResumableWith @extra (\ f -> extra (f ())) (const scopedHandler)

-- |Combined higher-order interpreter for 'Resumable' and 'Scoped'.
-- In this variant, only the handler may send 'Stop', but this allows resumption to happen on each action inside of the
-- scope.
interpretResumableScopedH ::
  ∀ resource effect err r .
  (∀ x . (resource -> Sem r x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) (Stop err : r) x) ->
  InterpreterFor (Scoped resource (effect !! err)) r
interpretResumableScopedH withResource scopedHandler =
  run
  where
    run :: InterpreterFor (Scoped resource (effect !! err)) r
    run =
      interpretH' \ (Weaving inner s' dist' ex' ins') -> case inner of
        Run resource (Resumable (Weaving effect s dist ex ins)) ->
          exFinal <$> runStop (tac (scopedHandler resource effect))
          where
            tac =
              runTactics
              (Compose (s <$ s'))
              (raise . raise . run . fmap Compose . dist' . fmap dist . getCompose)
              (ins <=< ins' . getCompose)
              (raise . run . fmap Compose . dist' . fmap dist . getCompose)
            exFinal = ex' . \case
              Right (Compose a) -> Right . ex <$> a
              Left err -> Left err <$ s'
        InScope main ->
          ex' <$> withResource \ resource -> run (dist' (main resource <$ s'))

-- |Combined interpreter for 'Resumable' and 'Scoped'.
-- In this variant, only the handler may send 'Stop', but this allows resumption to happen on each action inside of the
-- scope.
interpretResumableScoped ::
  ∀ resource effect err r .
  (∀ x . (resource -> Sem r x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : r) x) ->
  InterpreterFor (Scoped resource (effect !! err)) r
interpretResumableScoped withResource scopedHandler =
  interpretResumableScopedH withResource \ r e -> liftT (scopedHandler r e)

-- |Combined interpreter for 'Resumable' and 'Scoped'.
-- In this variant:
-- - Only the handler may send 'Stop', but this allows resumption to happen on each action inside of the scope.
-- - The resource allocator is a plain action.
interpretResumableScoped_ ::
  ∀ resource effect err r .
  Sem r resource ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : r) x) ->
  InterpreterFor (Scoped resource (effect !! err)) r
interpretResumableScoped_ withResource =
  interpretResumableScoped (=<< withResource)

-- |Combined higher-order interpreter for 'Resumable' and 'Scoped' that allows the handler to use additional effects
-- that are interpreted by the resource allocator.
-- In this variant, only the handler may send 'Stop', but this allows resumption to happen on each action inside of the
-- scope.
interpretResumableScopedWithH ::
  ∀ extra resource effect err r r1 .
  r1 ~ extra ++ r =>
  InsertAtIndex 1 '[Scoped resource (effect !! err)] r1 r (Scoped resource (effect !! err) : r1) extra =>
  (∀ x . (resource -> Sem r1 x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) (Stop err : r1) x) ->
  InterpreterFor (Scoped resource (effect !! err)) r
interpretResumableScopedWithH withResource scopedHandler =
  run
  where
    run :: InterpreterFor (Scoped resource (effect !! err)) r
    run =
      interpretH' \case
        Weaving (InScope main) s' dist' ex' _ ->
          ex' <$> withResource \ resource -> inScope (insertAt @1 @extra (dist' (main resource <$ s')))
        _ ->
          error "top level Run"
      where
        inScope :: InterpreterFor (Scoped resource (effect !! err)) r1
        inScope =
          interpretH' \case
            Weaving (Run resource (Resumable (Weaving effect s dist ex ins))) s' dist' ex' ins' ->
              exFinal <$> runStop (tac (scopedHandler resource effect))
              where
                tac =
                  runTactics
                  (Compose (s <$ s'))
                  (raise . raise . inScope . fmap Compose . dist' . fmap dist . getCompose)
                  (ins <=< ins' . getCompose)
                  (raise . inScope . fmap Compose . dist' . fmap dist . getCompose)
                exFinal = ex' . \case
                  Right (Compose a) -> Right . ex <$> a
                  Left err -> Left err <$ s'
            _ ->
              error "nested InScope"

-- |Combined interpreter for 'Resumable' and 'Scoped' that allows the handler to use additional effects that are
-- interpreted by the resource allocator.
-- In this variant, only the handler may send 'Stop', but this allows resumption to happen on each action inside of the
-- scope.
interpretResumableScopedWith ::
  ∀ extra resource effect err r r1 .
  r1 ~ extra ++ r =>
  InsertAtIndex 1 '[Scoped resource (effect !! err)] r1 r (Scoped resource (effect !! err) : r1) extra =>
  (∀ x . (resource -> Sem r1 x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : r1) x) ->
  InterpreterFor (Scoped resource (effect !! err)) r
interpretResumableScopedWith withResource scopedHandler =
  interpretResumableScopedWithH @extra withResource \ r e -> liftT (scopedHandler r e)

-- |Combined interpreter for 'Resumable' and 'Scoped' that allows the handler to use additional effects that are
-- interpreted by the resource allocator.
-- In this variant:
-- - Only the handler may send 'Stop', but this allows resumption to happen on each action inside of the scope.
-- - No resource is used and the allocator is a plain interpreter.
interpretResumableScopedWith_ ::
  ∀ extra effect err r r1 .
  r1 ~ extra ++ r =>
  InsertAtIndex 1 '[Scoped () (effect !! err)] r1 r (Scoped () (effect !! err) : r1) extra =>
  (∀ x . Sem r1 x -> Sem r x) ->
  (∀ r0 x . effect (Sem r0) x -> Sem (Stop err : r1) x) ->
  InterpreterFor (Scoped () (effect !! err)) r
interpretResumableScopedWith_ extra scopedHandler =
  interpretResumableScopedWith @extra @() @effect @err @r @r1 (\ f -> extra (f ())) (const scopedHandler)
