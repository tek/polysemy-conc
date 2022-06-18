{-# options_haddock prune #-}

-- |Description: PScoped Interpreters, Internal
module Polysemy.Conc.Interpreter.PScoped where

import Polysemy.Internal (Sem (Sem), liftSem, runSem)
import Polysemy.Internal.Index (InsertAtIndex)
import Polysemy.Internal.Tactics (liftT, runTactics)
import Polysemy.Internal.Union (Weaving (Weaving), decomp, hoist, injWeaving)
import Polysemy.Resume (Stop, runStop, type (!!))
import Polysemy.Resume.Effect.Resumable (Resumable (Resumable))

import Polysemy.Conc.Effect.PScoped (PScoped (InScope, Run))

interpretH' ::
  ∀ e r .
  (∀ x . Weaving e (Sem (e : r)) x -> Sem r x) ->
  InterpreterFor e r
interpretH' h (Sem m) =
  Sem \ k -> m $ decomp >>> \case
    Right wav -> runSem (h wav) k
    Left g -> k (hoist (interpretH' h) g)

-- |Interpreter for 'PScoped', taking a @resource@ allocation function and a parameterized interpreter for the plain
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
  runPScoped \ p use -> use =<< resource p

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
  interpretPScoped \ p use -> use =<< resource p

-- |Combined higher-order interpreter for 'PScoped' and 'Resumable'.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
interpretPScopedResumableH ::
  ∀ param resource effect err r .
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
              handlePScoped = \case
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
                (handlePScoped effect)
              exFinal = ex' . \case
                Right (Compose a) -> Right . ex <$> a
                Left err -> Left err <$ s'
            exFinal <$> runStop tac

-- |Combined interpreter for 'PScoped' and 'Resumable'.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
interpretPScopedResumable ::
  ∀ param resource effect err r .
  (∀ x . param -> (resource -> Sem (Stop err : r) x) -> Sem (Stop err : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : r) x) ->
  InterpreterFor (PScoped param resource effect !! err) r
interpretPScopedResumable withResource scopedHandler =
  interpretPScopedResumableH withResource \ r e -> liftT (scopedHandler r e)

-- |Combined interpreter for 'PScoped' and 'Resumable'.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
-- In this variant, the resource allocator is a plain action.
interpretPScopedResumable_ ::
  ∀ param resource effect err r .
  (param -> Sem (Stop err : r) resource) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : r) x) ->
  InterpreterFor (PScoped param resource effect !! err) r
interpretPScopedResumable_ resource =
  interpretPScopedResumable \ p use -> use =<< resource p

-- |Higher-order interpreter for 'PScoped' that allows the handler to use additional effects that are interpreted by the
-- resource allocator.
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

-- |Interpreter for 'PScoped' that allows the handler to use additional effects that are interpreted by the resource
-- allocator.
interpretPScopedWith ::
  ∀ extra param resource effect r r1 .
  r1 ~ (extra ++ r) =>
  InsertAtIndex 1 '[PScoped param resource effect] r1 r (PScoped param resource effect : r1) extra =>
  (∀ x . param -> (resource -> Sem r1 x) -> Sem r x) ->
  (∀ m x . resource -> effect m x -> Sem r1 x) ->
  InterpreterFor (PScoped param resource effect) r
interpretPScopedWith withResource scopedHandler =
  interpretPScopedWithH @extra withResource \ r e -> liftT (scopedHandler r e)

-- |Interpreter for 'PScoped' that allows the handler to use additional effects that are interpreted by the resource
-- allocator.
-- In this variant, no resource is used and the allocator is a plain interpreter.
interpretPScopedWith_ ::
  ∀ extra param effect r r1 .
  r1 ~ (extra ++ r) =>
  InsertAtIndex 1 '[PScoped param () effect] r1 r (PScoped param () effect : r1) extra =>
  (∀ x . param -> Sem r1 x -> Sem r x) ->
  (∀ m x . effect m x -> Sem r1 x) ->
  InterpreterFor (PScoped param () effect) r
interpretPScopedWith_ withResource scopedHandler =
  interpretPScopedWithH @extra (\ p f -> withResource p (f ())) \ () e -> liftT (scopedHandler e)

-- |Combined higher-order interpreter for 'PScoped' and 'Resumable' that allows the handler to use additional effects
-- that are interpreted by the resource allocator.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
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
              handlePScoped = \case
                Run _ _ ->
                  error "top level Run"
                InScope param main ->
                  raise $ withResource param \ resource ->
                    Compose <$> inScope (insertAt @1 @(extra ++ '[Stop err]) (dist' (dist (main resource <$ s) <$ s')))
              tac =
                runTactics
                (Compose (s <$ s'))
                (raise . raise . run . fmap Compose . dist' . fmap dist . getCompose)
                (ins <=< ins' . getCompose)
                (raise . run . fmap Compose . dist' . fmap dist . getCompose)
                (handlePScoped effect)
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
              handlePScoped = \case
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
                (handlePScoped effect)
              exFinal = ex' . \case
                Right (Compose a) -> Right . ex <$> a
                Left err -> Left err <$ s'
            exFinal <$> runStop (raise tac)

-- |Combined interpreter for 'PScoped' and 'Resumable' that allows the handler to use additional effects that are
-- interpreted by the resource allocator.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
interpretPScopedResumableWith ::
  ∀ extra param resource effect err r r1 .
  r1 ~ ((extra ++ '[Stop err]) ++ r) =>
  InsertAtIndex 1 '[PScoped param resource effect !! err] r1 r (PScoped param resource effect !! err : r1) (extra ++ '[Stop err]) =>
  (∀ x . param -> (resource -> Sem r1 x) -> Sem (Stop err : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem r1 x) ->
  InterpreterFor (PScoped param resource effect !! err) r
interpretPScopedResumableWith withResource scopedHandler =
  interpretPScopedResumableWithH @extra withResource \ r e -> liftT (scopedHandler r e)

-- |Combined interpreter for 'PScoped' and 'Resumable' that allows the handler to use additional effects that are
-- interpreted by the resource allocator.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
-- In this variant, no resource is used and the allocator is a plain interpreter.
interpretPScopedResumableWith_ ::
  ∀ extra param effect err r r1 .
  r1 ~ ((extra ++ '[Stop err]) ++ r) =>
  InsertAtIndex 1 '[PScoped param () effect !! err] r1 r (PScoped param () effect !! err : r1) (extra ++ '[Stop err]) =>
  (∀ x . param -> Sem r1 x -> Sem (Stop err : r) x) ->
  (∀ r0 x . effect (Sem r0) x -> Sem r1 x) ->
  InterpreterFor (PScoped param () effect !! err) r
interpretPScopedResumableWith_ extra scopedHandler =
  interpretPScopedResumableWith @extra (\ p f -> extra p (f ())) (const scopedHandler)

-- |Combined higher-order interpreter for 'Resumable' and 'PScoped'.
-- In this variant, only the handler may send 'Stop', but this allows resumption to happen on each action inside of the
-- scope.
interpretResumablePScopedH ::
  ∀ param resource effect err r .
  (∀ x . param -> (resource -> Sem r x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) (Stop err : r) x) ->
  InterpreterFor (PScoped param resource (effect !! err)) r
interpretResumablePScopedH withResource scopedHandler =
  run
  where
    run :: InterpreterFor (PScoped param resource (effect !! err)) r
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
        InScope param main ->
          ex' <$> withResource param \ resource -> run (dist' (main resource <$ s'))

-- |Combined interpreter for 'Resumable' and 'PScoped'.
-- In this variant, only the handler may send 'Stop', but this allows resumption to happen on each action inside of the
-- scope.
interpretResumablePScoped ::
  ∀ param resource effect err r .
  (∀ x . param -> (resource -> Sem r x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : r) x) ->
  InterpreterFor (PScoped param resource (effect !! err)) r
interpretResumablePScoped withResource scopedHandler =
  interpretResumablePScopedH withResource \ r e -> liftT (scopedHandler r e)

-- |Combined interpreter for 'Resumable' and 'PScoped'.
-- In this variant:
-- - Only the handler may send 'Stop', but this allows resumption to happen on each action inside of the scope.
-- - The resource allocator is a plain action.
interpretResumablePScoped_ ::
  ∀ param resource effect err r .
  (param -> Sem r resource) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : r) x) ->
  InterpreterFor (PScoped param resource (effect !! err)) r
interpretResumablePScoped_ resource =
  interpretResumablePScoped \ p use -> use =<< resource p

-- |Combined higher-order interpreter for 'Resumable' and 'PScoped' that allows the handler to use additional effects
-- that are interpreted by the resource allocator.
-- In this variant, only the handler may send 'Stop', but this allows resumption to happen on each action inside of the
-- scope.
interpretResumablePScopedWithH ::
  ∀ extra param resource effect err r r1 .
  r1 ~ (extra ++ r) =>
  InsertAtIndex 1 '[PScoped param resource (effect !! err)] r1 r (PScoped param resource (effect !! err) : r1) extra =>
  (∀ x . param -> (resource -> Sem r1 x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) (Stop err : r1) x) ->
  InterpreterFor (PScoped param resource (effect !! err)) r
interpretResumablePScopedWithH withResource scopedHandler =
  run
  where
    run :: InterpreterFor (PScoped param resource (effect !! err)) r
    run =
      interpretH' \case
        Weaving (InScope param main) s' dist' ex' _ ->
          ex' <$> withResource param \ resource -> inScope (insertAt @1 @extra (dist' (main resource <$ s')))
        _ ->
          error "top level Run"
      where
        inScope :: InterpreterFor (PScoped param resource (effect !! err)) r1
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

-- |Combined interpreter for 'Resumable' and 'PScoped' that allows the handler to use additional effects that are
-- interpreted by the resource allocator.
-- In this variant, only the handler may send 'Stop', but this allows resumption to happen on each action inside of the
-- scope.
interpretResumablePScopedWith ::
  ∀ extra param resource effect err r r1 .
  r1 ~ (extra ++ r) =>
  InsertAtIndex 1 '[PScoped param resource (effect !! err)] r1 r (PScoped param resource (effect !! err) : r1) extra =>
  (∀ x . param -> (resource -> Sem r1 x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : r1) x) ->
  InterpreterFor (PScoped param resource (effect !! err)) r
interpretResumablePScopedWith withResource scopedHandler =
  interpretResumablePScopedWithH @extra withResource \ r e -> liftT (scopedHandler r e)

-- |Combined interpreter for 'Resumable' and 'PScoped' that allows the handler to use additional effects that are
-- interpreted by the resource allocator.
-- In this variant:
-- - Only the handler may send 'Stop', but this allows resumption to happen on each action inside of the scope.
-- - No resource is used and the allocator is a plain interpreter.
interpretResumablePScopedWith_ ::
  ∀ extra param effect err r r1 .
  r1 ~ (extra ++ r) =>
  InsertAtIndex 1 '[PScoped param () (effect !! err)] r1 r (PScoped param () (effect !! err) : r1) extra =>
  (∀ x . param -> Sem r1 x -> Sem r x) ->
  (∀ r0 x . effect (Sem r0) x -> Sem (Stop err : r1) x) ->
  InterpreterFor (PScoped param () (effect !! err)) r
interpretResumablePScopedWith_ extra scopedHandler =
  interpretResumablePScopedWith @extra @param @() @effect @err @r @r1 (\ p f -> extra p (f ())) (const scopedHandler)
