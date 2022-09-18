{-# options_haddock prune #-}

-- |Description: Scoped Interpreters, Internal
module Polysemy.Conc.Interpreter.Scoped where

import Polysemy.Internal (Sem (Sem), liftSem, runSem)
import Polysemy.Internal.Index (InsertAtIndex)
import Polysemy.Internal.Tactics (liftT, runTactics)
import Polysemy.Internal.Union (Weaving (Weaving), decomp, hoist, injWeaving)
import Polysemy.Resume (Stop, runStop, type (!!))
import Polysemy.Resume.Effect.Resumable (Resumable (Resumable))

import Polysemy.Conc.Effect.Scoped (Scoped (InScope, Run))

interpretWeaving ::
  ∀ e r .
  (∀ x . Weaving e (Sem (e : r)) x -> Sem r x) ->
  InterpreterFor e r
interpretWeaving h (Sem m) =
  Sem \ k -> m $ decomp >>> \case
    Right wav -> runSem (h wav) k
    Left g -> k (hoist (interpretWeaving h) g)

-- |Interpreter for 'Scoped', taking a @resource@ allocation function and a parameterized interpreter for the plain
-- @effect@.
--
-- @withResource@ is a callback function, allowing the user to acquire the resource for each program from other effects.
--
-- @scopedInterpreter@ is a regular interpreter that is called with the @resource@ argument produced by @scope@.
-- /Note/: This function will be called for each action in the program, so if the interpreter allocates any resources,
-- they will be scoped to a single action. Move them to @withResource@ instead.
runScoped ::
  ∀ param resource effect r .
  (∀ x . param -> (resource -> Sem r x) -> Sem r x) ->
  (resource -> InterpreterFor effect r) ->
  InterpreterFor (Scoped param resource effect) r
runScoped withResource scopedInterpreter =
  run
  where
    run :: InterpreterFor (Scoped param resource effect) r
    run =
      interpretWeaving \ (Weaving effect s wv ex ins) -> case effect of
        Run resource act ->
          scopedInterpreter resource (liftSem $ injWeaving $ Weaving act s (raise . run . wv) ex ins)
        InScope param main ->
          withResource param \ resource -> ex <$> run (wv (main resource <$ s))

-- |Variant of 'runScoped' in which the resource allocator is a plain action.
runScopedAs ::
  ∀ param resource effect r .
  (param -> Sem r resource) ->
  (resource -> InterpreterFor effect r) ->
  InterpreterFor (Scoped param resource effect) r
runScopedAs resource =
  runScoped \ p use -> use =<< resource p

-- |Variant of 'runScoped' that takes a higher-order handler instead of an interpreter.
interpretScopedH ::
  ∀ param resource effect r .
  (∀ x . param -> (resource -> Sem r x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) r x) ->
  InterpreterFor (Scoped param resource effect) r
interpretScopedH withResource scopedHandler =
  run
  where
    run :: InterpreterFor (Scoped param resource effect) r
    run =
      interpretWeaving \ (Weaving effect s wv ex ins) -> case effect of
        Run resource act ->
          ex <$> runTactics s (raise . run . wv) ins (run . wv) (scopedHandler resource act)
        InScope param main ->
          withResource param \ resource -> ex <$> run (wv (main resource <$ s))

-- |Variant of 'runScoped' that takes a higher-order handler instead of an interpreter.
interpretScopedH' ::
  ∀ param resource effect r .
  (∀ e m x . param -> (resource -> Tactical e m r x) -> Tactical e m r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical (Scoped param resource effect) (Sem r0) r x) ->
  InterpreterFor (Scoped param resource effect) r
interpretScopedH' withResource scopedHandler =
  interpretH \case
    Run resource act ->
      scopedHandler resource act
    InScope param main ->
      withResource param \ resource ->
        runTSimple (main resource)

-- |Variant of 'runScoped' that takes a handler instead of an interpreter.
interpretScoped ::
  ∀ param resource effect r .
  (∀ x . param -> (resource -> Sem r x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem r x) ->
  InterpreterFor (Scoped param resource effect) r
interpretScoped withResource scopedHandler =
  interpretScopedH withResource \ r e -> liftT (scopedHandler r e)

-- |Variant of 'interpretScoped' in which the resource allocator is a plain action.
interpretScopedAs ::
  ∀ param resource effect r .
  (param -> Sem r resource) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem r x) ->
  InterpreterFor (Scoped param resource effect) r
interpretScopedAs resource =
  interpretScoped \ p use -> use =<< resource p

-- |Combined higher-order interpreter for 'Scoped' and 'Resumable'.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
interpretScopedResumableH ::
  ∀ param resource effect err r .
  (∀ x . param -> (resource -> Sem (Stop err : r) x) -> Sem (Stop err : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) (Stop err : r) x) ->
  InterpreterFor (Scoped param resource effect !! err) r
interpretScopedResumableH withResource scopedHandler =
  run
  where
    run :: InterpreterFor (Scoped param resource effect !! err) r
    run =
      interpretWeaving \ (Weaving (Resumable inner) s' dist' ex' ins') ->
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
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
interpretScopedResumable ::
  ∀ param resource effect err r .
  (∀ x . param -> (resource -> Sem (Stop err : r) x) -> Sem (Stop err : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : r) x) ->
  InterpreterFor (Scoped param resource effect !! err) r
interpretScopedResumable withResource scopedHandler =
  interpretScopedResumableH withResource \ r e -> liftT (scopedHandler r e)

-- |Combined interpreter for 'Scoped' and 'Resumable'.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
-- In this variant, the resource allocator is a plain action.
interpretScopedResumable_ ::
  ∀ param resource effect err r .
  (param -> Sem (Stop err : r) resource) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : r) x) ->
  InterpreterFor (Scoped param resource effect !! err) r
interpretScopedResumable_ resource =
  interpretScopedResumable \ p use -> use =<< resource p

-- |Higher-order interpreter for 'Scoped' that allows the handler to use additional effects that are interpreted by the
-- resource allocator.
interpretScopedWithH ::
  ∀ extra param resource effect r r1 .
  r1 ~ (extra ++ r) =>
  InsertAtIndex 0 '[] r1 r r1 extra =>
  InsertAtIndex 1 '[Scoped param resource effect] r1 r (Scoped param resource effect : r1) extra =>
  (∀ x . param -> (resource -> Sem r1 x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) r1 x) ->
  InterpreterFor (Scoped param resource effect) r
interpretScopedWithH withResource scopedHandler =
  interpretWeaving \case
    Weaving (InScope param main) s wv ex _ ->
      ex <$> withResource param \ resource -> inScope (insertAt @1 @extra (wv (main resource <$ s)))
    _ ->
      error "top level Run"
  where
    inScope :: InterpreterFor (Scoped param resource effect) r1
    inScope =
      interpretWeaving \case
        Weaving (InScope param main) s wv ex _ ->
          insertAt @0 @extra (ex <$> withResource param \ resource -> inScope (wv (main resource <$ s)))
        Weaving (Run resource act) s wv ex ins ->
          ex <$> runTactics s (raise . inScope . wv) ins (inScope . wv) (scopedHandler resource act)

-- |Interpreter for 'Scoped' that allows the handler to use additional effects that are interpreted by the resource
-- allocator.
interpretScopedWith ::
  ∀ extra param resource effect r r1 .
  r1 ~ (extra ++ r) =>
  InsertAtIndex 0 '[] r1 r r1 extra =>
  InsertAtIndex 1 '[Scoped param resource effect] r1 r (Scoped param resource effect : r1) extra =>
  (∀ x . param -> (resource -> Sem r1 x) -> Sem r x) ->
  (∀ m x . resource -> effect m x -> Sem r1 x) ->
  InterpreterFor (Scoped param resource effect) r
interpretScopedWith withResource scopedHandler =
  interpretScopedWithH @extra withResource \ r e -> liftT (scopedHandler r e)

-- |Interpreter for 'Scoped' that allows the handler to use additional effects that are interpreted by the resource
-- allocator.
-- In this variant, no resource is used and the allocator is a plain interpreter.
interpretScopedWith_ ::
  ∀ extra param effect r r1 .
  r1 ~ (extra ++ r) =>
  InsertAtIndex 0 '[] r1 r r1 extra =>
  InsertAtIndex 1 '[Scoped param () effect] r1 r (Scoped param () effect : r1) extra =>
  (∀ x . param -> Sem r1 x -> Sem r x) ->
  (∀ m x . effect m x -> Sem r1 x) ->
  InterpreterFor (Scoped param () effect) r
interpretScopedWith_ withResource scopedHandler =
  interpretScopedWithH @extra (\ p f -> withResource p (f ())) \ () e -> liftT (scopedHandler e)

-- |Combined higher-order interpreter for 'Scoped' and 'Resumable' that allows the handler to use additional effects
-- that are interpreted by the resource allocator.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
interpretScopedResumableWithH ::
  ∀ extra param resource effect err r r1 .
  r1 ~ (extra ++ (Stop err : r)) =>
  r1 ~ ((extra ++ '[Stop err]) ++ r) =>
  InsertAtIndex 0 '[] r1 (Stop err : r) r1 extra =>
  InsertAtIndex 1 '[Scoped param resource effect !! err] r1 r (Scoped param resource effect !! err : r1) (extra ++ '[Stop err]) =>
  (∀ x . param -> (resource -> Sem r1 x) -> Sem (Stop err : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) r1 x) ->
  InterpreterFor (Scoped param resource effect !! err) r
interpretScopedResumableWithH withResource scopedHandler =
  run
  where
    run :: InterpreterFor (Scoped param resource effect !! err) r
    run =
      interpretWeaving \ (Weaving (Resumable inner) s' dist' ex' ins') ->
        case inner of
          Weaving effect s dist ex ins -> do
            let
              handleScoped = \case
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
                (handleScoped effect)
              exFinal = ex' . \case
                Right (Compose a) -> Right . ex <$> a
                Left err -> Left err <$ s'
            exFinal <$> runStop tac
    inScope :: InterpreterFor (Scoped param resource effect !! err) r1
    inScope =
      interpretWeaving \ (Weaving (Resumable inner) s' dist' ex' ins') ->
        case inner of
          Weaving effect s dist ex ins -> do
            let
              handleScoped = \case
                Run resource act ->
                  scopedHandler resource act
                InScope param main ->
                  raise $ insertAt @0 @extra $ withResource param \ resource ->
                    Compose <$> inScope (dist' (dist (main resource <$ s) <$ s'))
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
  ∀ extra param resource effect err r r1 .
  r1 ~ (extra ++ (Stop err : r)) =>
  r1 ~ ((extra ++ '[Stop err]) ++ r) =>
  InsertAtIndex 0 '[] r1 (Stop err : r) r1 extra =>
  InsertAtIndex 1 '[Scoped param resource effect !! err] r1 r (Scoped param resource effect !! err : r1) (extra ++ '[Stop err]) =>
  (∀ x . param -> (resource -> Sem r1 x) -> Sem (Stop err : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem r1 x) ->
  InterpreterFor (Scoped param resource effect !! err) r
interpretScopedResumableWith withResource scopedHandler =
  interpretScopedResumableWithH @extra withResource \ r e -> liftT (scopedHandler r e)

-- |Combined interpreter for 'Scoped' and 'Resumable' that allows the handler to use additional effects that are
-- interpreted by the resource allocator.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
-- In this variant, no resource is used and the allocator is a plain interpreter.
interpretScopedResumableWith_ ::
  ∀ extra param effect err r r1 .
  r1 ~ (extra ++ (Stop err : r)) =>
  r1 ~ ((extra ++ '[Stop err]) ++ r) =>
  InsertAtIndex 0 '[] r1 (Stop err : r) r1 extra =>
  InsertAtIndex 1 '[Scoped param () effect !! err] r1 r (Scoped param () effect !! err : r1) (extra ++ '[Stop err]) =>
  (∀ x . param -> Sem r1 x -> Sem (Stop err : r) x) ->
  (∀ r0 x . effect (Sem r0) x -> Sem r1 x) ->
  InterpreterFor (Scoped param () effect !! err) r
interpretScopedResumableWith_ extra scopedHandler =
  interpretScopedResumableWith @extra (\ p f -> extra p (f ())) (const scopedHandler)

-- |Combined higher-order interpreter for 'Resumable' and 'Scoped'.
-- In this variant, only the handler may send 'Stop', but this allows resumption to happen on each action inside of the
-- scope.
interpretResumableScopedH ::
  ∀ param resource effect err r .
  (∀ x . param -> (resource -> Sem r x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) (Stop err : r) x) ->
  InterpreterFor (Scoped param resource (effect !! err)) r
interpretResumableScopedH withResource scopedHandler =
  run
  where
    run :: InterpreterFor (Scoped param resource (effect !! err)) r
    run =
      interpretWeaving \ (Weaving inner s' dist' ex' ins') -> case inner of
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

-- |Combined interpreter for 'Resumable' and 'Scoped'.
-- In this variant, only the handler may send 'Stop', but this allows resumption to happen on each action inside of the
-- scope.
interpretResumableScoped ::
  ∀ param resource effect err r .
  (∀ x . param -> (resource -> Sem r x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : r) x) ->
  InterpreterFor (Scoped param resource (effect !! err)) r
interpretResumableScoped withResource scopedHandler =
  interpretResumableScopedH withResource \ r e -> liftT (scopedHandler r e)

-- |Combined interpreter for 'Resumable' and 'Scoped'.
-- In this variant:
-- - Only the handler may send 'Stop', but this allows resumption to happen on each action inside of the scope.
-- - The resource allocator is a plain action.
interpretResumableScoped_ ::
  ∀ param resource effect err r .
  (param -> Sem r resource) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : r) x) ->
  InterpreterFor (Scoped param resource (effect !! err)) r
interpretResumableScoped_ resource =
  interpretResumableScoped \ p use -> use =<< resource p

-- |Combined higher-order interpreter for 'Resumable' and 'Scoped' that allows the handler to use additional effects
-- that are interpreted by the resource allocator.
-- In this variant, only the handler may send 'Stop', but this allows resumption to happen on each action inside of the
-- scope.
interpretResumableScopedWithH ::
  ∀ extra param resource effect err r r1 .
  r1 ~ (extra ++ r) =>
  InsertAtIndex 0 '[] r1 r r1 extra =>
  InsertAtIndex 1 '[Scoped param resource (effect !! err)] r1 r (Scoped param resource (effect !! err) : r1) extra =>
  (∀ x . param -> (resource -> Sem r1 x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) (Stop err : r1) x) ->
  InterpreterFor (Scoped param resource (effect !! err)) r
interpretResumableScopedWithH withResource scopedHandler =
  run
  where
    run :: InterpreterFor (Scoped param resource (effect !! err)) r
    run =
      interpretWeaving \case
        Weaving (InScope param main) s dist ex _ ->
          ex <$> withResource param \ resource -> inScope (insertAt @1 @extra (dist (main resource <$ s)))
        _ ->
          error "top level Run"
      where
        inScope :: InterpreterFor (Scoped param resource (effect !! err)) r1
        inScope =
          interpretWeaving \case
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
            Weaving (InScope param main) s wv ex _ ->
              insertAt @0 @extra (ex <$> withResource param \ resource -> inScope (wv (main resource <$ s)))

-- |Combined interpreter for 'Resumable' and 'Scoped' that allows the handler to use additional effects that are
-- interpreted by the resource allocator.
-- In this variant, only the handler may send 'Stop', but this allows resumption to happen on each action inside of the
-- scope.
interpretResumableScopedWith ::
  ∀ extra param resource effect err r r1 .
  r1 ~ (extra ++ r) =>
  InsertAtIndex 0 '[] r1 r r1 extra =>
  InsertAtIndex 1 '[Scoped param resource (effect !! err)] r1 r (Scoped param resource (effect !! err) : r1) extra =>
  (∀ x . param -> (resource -> Sem r1 x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : r1) x) ->
  InterpreterFor (Scoped param resource (effect !! err)) r
interpretResumableScopedWith withResource scopedHandler =
  interpretResumableScopedWithH @extra withResource \ r e -> liftT (scopedHandler r e)

-- |Combined interpreter for 'Resumable' and 'Scoped' that allows the handler to use additional effects that are
-- interpreted by the resource allocator.
-- In this variant:
-- - Only the handler may send 'Stop', but this allows resumption to happen on each action inside of the scope.
-- - No resource is used and the allocator is a plain interpreter.
interpretResumableScopedWith_ ::
  ∀ extra param effect err r r1 .
  r1 ~ (extra ++ r) =>
  InsertAtIndex 0 '[] r1 r r1 extra =>
  InsertAtIndex 1 '[Scoped param () (effect !! err)] r1 r (Scoped param () (effect !! err) : r1) extra =>
  (∀ x . param -> Sem r1 x -> Sem r x) ->
  (∀ r0 x . effect (Sem r0) x -> Sem (Stop err : r1) x) ->
  InterpreterFor (Scoped param () (effect !! err)) r
interpretResumableScopedWith_ extra scopedHandler =
  interpretResumableScopedWith @extra @param @() @effect @err @r @r1 (\ p f -> extra p (f ())) (const scopedHandler)

-- |Combined higher-order interpreter for 'Resumable' and 'Scoped'.
-- In this variant, both the handler and the scope may send different errors via 'Stop', encoding the concept that the
-- resource allocation may fail to prevent the scope from being executed, and each individual scoped action may fail,
-- continuing the scope execution on resumption.
interpretScopedRH ::
  ∀ param resource effect eo ei r .
  (∀ x . param -> (resource -> Sem (Stop eo : r) x) -> Sem (Stop eo : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) (Stop ei : r) x) ->
  InterpreterFor (Scoped param resource (effect !! ei) !! eo) r
interpretScopedRH withResource scopedHandler =
  run
  where
    run :: InterpreterFor (Scoped param resource (effect !! ei) !! eo) r
    run =
      interpretWeaving \ (Weaving (Resumable (Weaving inner s' dist' ex' ins')) s'' dist'' ex'' ins'') -> case inner of
        Run resource (Resumable (Weaving effect s dist ex ins)) ->
          exFinal <$> runStop @ei (tac (scopedHandler resource effect))
          where
            tac =
              runTactics
              (Compose (Compose (s <$ s') <$ s''))
              (raise . raise . run . fmap Compose . fmap (fmap Compose) . dist'' . fmap dist' . fmap (fmap dist . getCompose) . getCompose)
              (ins <=< ins' . getCompose <=< ins'' . getCompose)
              (raise . run . fmap Compose . fmap (fmap Compose) . dist'' . fmap dist' . fmap (fmap dist . getCompose) . getCompose)
            exFinal = \case
              Right (Compose fffa) ->
                ex'' $ fffa <&> Right . \ (Compose ffa) ->
                  ex' (Right . ex <$> ffa)
              Left err ->
                ex'' (Right (ex' (Left err <$ s')) <$ s'')
        InScope param main -> do
          let
            inScope =
                raise (withResource param \ resource -> Compose <$> raise (run (dist'' (dist' (main resource <$ s') <$ s''))))
            tac =
              runTactics
              (Compose (s' <$ s''))
              (raise . raise . run . fmap Compose . dist'' . fmap dist' . getCompose)
              (ins' <=< ins'' . getCompose)
              (raise . run . fmap Compose . dist'' . fmap dist' . getCompose)
              inScope
            exFinal = ex'' . \case
              Right (Compose a) -> Right . ex' <$> a
              Left err -> Left err <$ s''
          exFinal <$> runStop tac

-- |Combined interpreter for 'Scoped' and 'Resumable'.
-- In this variant, both the handler and the scope may send different errors via 'Stop', encoding the concept that the
-- resource allocation may fail to prevent the scope from being executed, and each individual scoped action may fail,
-- continuing the scope execution on resumption.
interpretScopedR ::
  ∀ param resource effect eo ei r .
  (∀ x . param -> (resource -> Sem (Stop eo : r) x) -> Sem (Stop eo : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop ei : r) x) ->
  InterpreterFor (Scoped param resource (effect !! ei) !! eo) r
interpretScopedR withResource scopedHandler =
  interpretScopedRH withResource \ r e -> liftT (scopedHandler r e)

-- |Combined interpreter for 'Scoped' and 'Resumable'.
-- In this variant:
-- - Both the handler and the scope may send different errors via 'Stop', encoding the concept that the
--   resource allocation may fail to prevent the scope from being executed, and each individual scoped action may fail,
--   continuing the scope execution on resumption.
-- - The resource allocator is a plain action.
interpretScopedR_ ::
  ∀ param resource effect eo ei r .
  (param -> Sem (Stop eo : r) resource) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop ei : r) x) ->
  InterpreterFor (Scoped param resource (effect !! ei) !! eo) r
interpretScopedR_ resource =
  interpretScopedR \ p use -> use =<< resource p

-- |Combined higher-order interpreter for 'Scoped' and 'Resumable' that allows the handler to use additional effects
-- that are interpreted by the resource allocator.
-- In this variant, both the handler and the scope may send different errors via 'Stop', encoding the concept that the
-- resource allocation may fail to prevent the scope from being executed, and each individual scoped action may fail,
-- continuing the scope execution on resumption.
interpretScopedRWithH ::
  ∀ extra param resource effect eo ei r r1 .
  r1 ~ (extra ++ Stop eo : r) =>
  r1 ~ ((extra ++ '[Stop eo]) ++ r) =>
  InsertAtIndex 0 '[] (Stop eo : r1) (Stop eo : r) (Stop eo : r1) (Stop eo : extra) =>
  InsertAtIndex 1 '[Scoped param resource (effect !! ei) !! eo] r1 r (Scoped param resource (effect !! ei) !! eo : r1) (extra ++ '[Stop eo]) =>
  (∀ x . param -> (resource -> Sem r1 x) -> Sem (Stop eo : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) (Stop ei : r1) x) ->
  InterpreterFor (Scoped param resource (effect !! ei) !! eo) r
interpretScopedRWithH withResource scopedHandler =
  run
  where
    run :: InterpreterFor (Scoped param resource (effect !! ei) !! eo) r
    run =
      interpretWeaving \ (Weaving (Resumable (Weaving inner s' dist' ex' ins')) s'' dist'' ex'' ins'') -> case inner of
        InScope param main -> do
            let
              ma =
                  raise $ withResource param \ resource ->
                    Compose <$> inScope (insertAt @1 @(extra ++ '[Stop eo]) (dist'' (dist' (main resource <$ s') <$ s'')))
              tac =
                runTactics
                (Compose (s' <$ s''))
                (raise . raise . run . fmap Compose . dist'' . fmap dist' . getCompose)
                (ins' <=< ins'' . getCompose)
                (raise . run . fmap Compose . dist'' . fmap dist' . getCompose)
                ma
              exFinal = ex'' . \case
                Right (Compose a) -> Right . ex' <$> a
                Left err -> Left err <$ s''
            exFinal <$> runStop tac
        _ ->
          error "top level Run"
    inScope :: InterpreterFor (Scoped param resource (effect !! ei) !! eo) r1
    inScope =
      interpretWeaving \ (Weaving (Resumable (Weaving inner s' dist' ex' ins')) s'' dist'' ex'' ins'') -> case inner of
        Run resource (Resumable (Weaving effect s dist ex ins)) ->
          exFinal <$> runStop @ei (tac (scopedHandler resource effect))
          where
            tac =
              runTactics
              (Compose (Compose (s <$ s') <$ s''))
              (raise . raise . inScope . fmap Compose . fmap (fmap Compose) . dist'' . fmap dist' . fmap (fmap dist . getCompose) . getCompose)
              (ins <=< ins' . getCompose <=< ins'' . getCompose)
              (raise . inScope . fmap Compose . fmap (fmap Compose) . dist'' . fmap dist' . fmap (fmap dist . getCompose) . getCompose)
            exFinal = \case
              Right (Compose fffa) ->
                ex'' $ fffa <&> Right . \ (Compose ffa) ->
                  ex' (Right . ex <$> ffa)
              Left err ->
                ex'' (Right (ex' (Left err <$ s')) <$ s'')
        InScope param main -> do
            let
              ma =
                  raise $ insertAt @0 @(Stop eo : extra) $ withResource param \ resource ->
                    Compose <$> inScope (dist'' (dist' (main resource <$ s') <$ s''))
              tac =
                runTactics
                (Compose (s' <$ s''))
                (raise . raise . inScope . fmap Compose . dist'' . fmap dist' . getCompose)
                (ins' <=< ins'' . getCompose)
                (raise . inScope . fmap Compose . dist'' . fmap dist' . getCompose)
                ma
              exFinal = ex'' . \case
                Right (Compose a) -> Right . ex' <$> a
                Left err -> Left err <$ s''
            exFinal <$> runStop tac

-- |Combined interpreter for 'Scoped' and 'Resumable' that allows the handler to use additional effects that are
-- interpreted by the resource allocator.
-- In this variant, both the handler and the scope may send different errors via 'Stop', encoding the concept that the
-- resource allocation may fail to prevent the scope from being executed, and each individual scoped action may fail,
-- continuing the scope execution on resumption.
interpretScopedRWith ::
  ∀ extra param resource effect eo ei r r1 .
  r1 ~ (extra ++ Stop eo : r) =>
  r1 ~ ((extra ++ '[Stop eo]) ++ r) =>
  InsertAtIndex 0 '[] (Stop eo : r1) (Stop eo : r) (Stop eo : r1) (Stop eo : extra) =>
  InsertAtIndex 1 '[Scoped param resource (effect !! ei) !! eo] r1 r (Scoped param resource (effect !! ei) !! eo : r1) (extra ++ '[Stop eo]) =>
  (∀ x . param -> (resource -> Sem r1 x) -> Sem (Stop eo : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop ei : r1) x) ->
  InterpreterFor (Scoped param resource (effect !! ei) !! eo) r
interpretScopedRWith withResource scopedHandler =
  interpretScopedRWithH @extra withResource \ r e -> liftT (scopedHandler r e)

-- |Combined interpreter for 'Scoped' and 'Resumable' that allows the handler to use additional effects that are
-- interpreted by the resource allocator.
-- - Both the handler and the scope may send different errors via 'Stop', encoding the concept that the
--   resource allocation may fail to prevent the scope from being executed, and each individual scoped action may fail,
--   continuing the scope execution on resumption.
-- - The resource allocator is a plain action.
interpretScopedRWith_ ::
  ∀ extra param effect eo ei r r1 .
  r1 ~ (extra ++ Stop eo : r) =>
  r1 ~ ((extra ++ '[Stop eo]) ++ r) =>
  InsertAtIndex 0 '[] (Stop eo : r1) (Stop eo : r) (Stop eo : r1) (Stop eo : extra) =>
  InsertAtIndex 1 '[Scoped param () (effect !! ei) !! eo] r1 r (Scoped param () (effect !! ei) !! eo : r1) (extra ++ '[Stop eo]) =>
  (∀ x . param -> Sem r1 x -> Sem (Stop eo : r) x) ->
  (∀ r0 x . effect (Sem r0) x -> Sem (Stop ei : r1) x) ->
  InterpreterFor (Scoped param () (effect !! ei) !! eo) r
interpretScopedRWith_ extra scopedHandler =
  interpretScopedRWith @extra (\ p f -> extra p (f ())) (const scopedHandler)
