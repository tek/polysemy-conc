{-# options_haddock prune #-}

-- | Description: Scoped Interpreters, Internal
module Polysemy.Conc.Interpreter.Scoped where

import GHC.Err (errorWithoutStackTrace)
import Polysemy.Internal (Sem (Sem), hoistSem, liftSem, runSem)
import Polysemy.Internal.Sing (KnownList (singList))
import Polysemy.Internal.Tactics (liftT)
import Polysemy.Internal.Union (
  Union (Union),
  Weaving (Weaving),
  decomp,
  extendMembershipLeft,
  hoist,
  injWeaving,
  injectMembership,
  weave,
  )
import Polysemy.Membership (ElemOf)
import Polysemy.Resume (Stop, interpretResumableH, runStop, type (!!))
import Polysemy.Resume.Effect.Resumable (Resumable (Resumable))
import Prelude hiding (Scoped, interpretScoped, interpretScopedH, interpretScopedWithH, runScoped)

import Polysemy.Conc.Effect.Scoped (Scoped (InScope, Run))

interpretWeaving ::
  ∀ e r .
  (∀ x . Weaving e (Sem (e : r)) x -> Sem r x) ->
  InterpreterFor e r
interpretWeaving h (Sem m) =
  Sem \ k -> m $ decomp >>> \case
    Right wav -> runSem (h wav) k
    Left g -> k (hoist (interpretWeaving h) g)

restack :: (∀ e . ElemOf e r -> ElemOf e r')
        -> Sem r a
        -> Sem r' a
restack n =
  hoistSem $ \ (Union pr wav) -> hoist (restack n) (Union (n pr) wav)
{-# inline restack #-}

exResumable ::
  Functor f =>
  f () ->
  (f (Either err a) -> x) ->
  (f' a' -> a) ->
  Sem r (Either err (f (f' a'))) ->
  Sem r x
exResumable s ex ex' =
  fmap $ ex . \case
    Right a -> Right . ex' <$> a
    Left err -> Left err <$ s
{-# inline exResumable #-}

-- | Construct an interpreter for a higher-order effect wrapped in a 'Scoped',
-- given a resource allocation function and a parameterized handler for the
-- plain effect.
--
-- This combinator is analogous to 'interpretH' in that it allows the handler to
-- use the 'Tactical' environment and transforms the effect into other effects
-- on the stack.
interpretScopedH ::
  ∀ resource param effect r .
  -- | A callback function that allows the user to acquire a resource for each
  -- computation wrapped by 'Polysemy.Conc.scoped' using other effects, with an additional
  -- argument that contains the call site parameter passed to 'Polysemy.Conc.scoped'.
  (∀ x . param -> (resource -> Sem r x) -> Sem r x) ->
  -- | A handler like the one expected by 'interpretH' with an additional
  -- parameter that contains the @resource@ allocated by the first argument.
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) r x) ->
  InterpreterFor (Scoped param effect) r
interpretScopedH withResource scopedHandler =
  interpretWeaving $ \(Weaving effect s wv ex _) -> case effect of
    Run _ -> errorWithoutStackTrace "top level run"
    InScope param main -> withResource param \ resource ->
          ex
      <$> interpretH (scopedHandler resource) (go $ raiseUnder $ wv (main <$ s))
  where
    -- TODO investigate whether loopbreaker optimization is effective here
    go :: InterpreterFor (Scoped param effect) (effect : r)
    go =
      interpretWeaving \ (Weaving effect s wv ex ins) -> case effect of
        Run act -> liftSem $ injWeaving $ Weaving act s (go . wv) ex ins
        InScope param main -> raise $ withResource param \ resource ->
          ex <$> interpretH (scopedHandler resource) (go $ wv (main <$ s))
{-# inline interpretScopedH #-}

-- | Variant of 'interpretScopedH' that allows the resource acquisition function
-- to use 'Tactical'.
interpretScopedH' ::
  ∀ resource param effect r .
  (∀ e r0 x . param -> (resource -> Tactical e (Sem r0) r x) ->
    Tactical e (Sem r0) r x) ->
  (∀ r0 x .
    resource -> effect (Sem r0) x ->
    Tactical (Scoped param effect) (Sem r0) r x) ->
  InterpreterFor (Scoped param effect) r
interpretScopedH' withResource scopedHandler =
  go (errorWithoutStackTrace "top level run")
  where
    go :: resource -> InterpreterFor (Scoped param effect) r
    go resource =
      interpretH \case
        Run act ->
          scopedHandler resource act
        InScope param main ->
          withResource param \ resource' ->
            raise . go resource' =<< runT main
{-# inline interpretScopedH' #-}

-- | First-order variant of 'interpretScopedH'.
interpretScoped ::
  ∀ resource param effect r .
  (∀ x . param -> (resource -> Sem r x) -> Sem r x) ->
  (∀ m x . resource -> effect m x -> Sem r x) ->
  InterpreterFor (Scoped param effect) r
interpretScoped withResource scopedHandler =
  interpretScopedH withResource \ r e -> liftT (scopedHandler r e)
{-# inline interpretScoped #-}

-- | Variant of 'interpretScoped' in which the resource allocator is a plain
-- action.
interpretScopedAs ::
  ∀ resource param effect r .
  (param -> Sem r resource) ->
  (∀ m x . resource -> effect m x -> Sem r x) ->
  InterpreterFor (Scoped param effect) r
interpretScopedAs resource =
  interpretScoped \ p use -> use =<< resource p
{-# inline interpretScopedAs #-}

-- | Higher-order interpreter for 'Scoped' that allows the handler to use
-- additional effects that are interpreted by the resource allocator.
--
-- /Note/: It is necessary to specify the list of local interpreters with a type
-- application; GHC won't be able to figure them out from the type of
-- @withResource@.
--
-- As an example for a higher order effect, consider a mutexed concurrent state
-- effect, where an effectful function may lock write access to the state while
-- making it still possible to read it:
--
-- > data MState s :: Effect where
-- >   MState :: (s -> m (s, a)) -> MState s m a
-- >   MRead :: MState s m s
-- >
-- > makeSem ''MState
--
-- We can now use an 'Polysemy.AtomicState.AtomicState' to store the current
-- value and lock write access with an @MVar@. Since the state callback is
-- effectful, we need a higher order interpreter:
--
-- > withResource ::
-- >   Member (Embed IO) r =>
-- >   s ->
-- >   (MVar () -> Sem (AtomicState s : r) a) ->
-- >   Sem r a
-- > withResource initial use = do
-- >   tv <- embed (newTVarIO initial)
-- >   lock <- embed (newMVar ())
-- >   runAtomicStateTVar tv $ use lock
-- >
-- > interpretMState ::
-- >   ∀ s r .
-- >   Members [Resource, Embed IO] r =>
-- >   InterpreterFor (Scoped s (MState s)) r
-- > interpretMState =
-- >   interpretScopedWithH @'[AtomicState s] withResource \ lock -> \case
-- >     MState f ->
-- >       bracket_ (embed (takeMVar lock)) (embed (tryPutMVar lock ())) do
-- >         s0 <- atomicGet
-- >         res <- runTSimple (f s0)
-- >         Inspector ins <- getInspectorT
-- >         for_ (ins res) \ (s, _) -> atomicPut s
-- >         pure (snd <$> res)
-- >     MRead ->
-- >       liftT atomicGet
interpretScopedWithH ::
  ∀ extra resource param effect r r1 .
  KnownList extra =>
  r1 ~ (extra ++ r) =>
  (∀ x . param -> (resource -> Sem r1 x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) r1 x) ->
  InterpreterFor (Scoped param effect) r
interpretScopedWithH withResource scopedHandler =
  interpretWeaving \case
    Weaving (InScope param main) s wv ex _ ->
      ex <$> withResource param \ resource ->
        interpretH (scopedHandler resource) $ inScope $
          restack
            (injectMembership
             (singList @'[Scoped param effect])
             (singList @(effect : extra))) $ wv (main <$ s)
    _ ->
      errorWithoutStackTrace "top level Run"
  where
    inScope :: InterpreterFor (Scoped param effect) (effect : r1)
    inScope =
      interpretWeaving \case
        Weaving (InScope param main) s wv ex _ ->
          restack
            (extendMembershipLeft (singList @(effect : extra)))
            (ex <$> withResource param \resource ->
                interpretH (scopedHandler resource) $ inScope $ wv (main <$ s))
        Weaving (Run act) s wv ex ins ->
          liftSem $ injWeaving $ Weaving act s (inScope . wv) ex ins
{-# inline interpretScopedWithH #-}

-- | First-order variant of 'interpretScopedWithH'.
--
-- /Note/: It is necessary to specify the list of local interpreters with a type
-- application; GHC won't be able to figure them out from the type of
-- @withResource@:
--
-- > data SomeAction :: Effect where
-- >   SomeAction :: SomeAction m ()
-- >
-- > foo :: InterpreterFor (Scoped () SomeAction) r
-- > foo =
-- >   interpretScopedWith @[Reader Int, State Bool] localEffects \ () -> \case
-- >     SomeAction -> put . (> 0) =<< ask @Int
-- >   where
-- >     localEffects () use = evalState False (runReader 5 (use ()))
interpretScopedWith ::
  ∀ extra param resource effect r r1 .
  r1 ~ (extra ++ r) =>
  KnownList extra =>
  (∀ x . param -> (resource -> Sem r1 x) -> Sem r x) ->
  (∀ m x . resource -> effect m x -> Sem r1 x) ->
  InterpreterFor (Scoped param effect) r
interpretScopedWith withResource scopedHandler =
  interpretScopedWithH @extra withResource \ r e -> liftT (scopedHandler r e)
{-# inline interpretScopedWith #-}

-- | Variant of 'interpretScopedWith' in which no resource is used and the
-- resource allocator is a plain interpreter.
-- This is useful for scopes that only need local effects, but no resources in
-- the handler.
--
-- See the /Note/ on 'interpretScopedWithH'.
interpretScopedWith_ ::
  ∀ extra param effect r r1 .
  r1 ~ (extra ++ r) =>
  KnownList extra =>
  (∀ x . param -> Sem r1 x -> Sem r x) ->
  (∀ m x . effect m x -> Sem r1 x) ->
  InterpreterFor (Scoped param effect) r
interpretScopedWith_ withResource scopedHandler =
  interpretScopedWithH @extra (\ p f -> withResource p (f ())) \ () e -> liftT (scopedHandler e)
{-# inline interpretScopedWith_ #-}

-- | Variant of 'interpretScoped' that uses another interpreter instead of a
-- handler.
--
-- This is mostly useful if you want to reuse an interpreter that you cannot
-- easily rewrite (like from another library). If you have full control over the
-- implementation, 'interpretScoped' should be preferred.
--
-- /Note/: In previous versions of Polysemy, the wrapped interpreter was
-- executed fully, including the initializing code surrounding its handler,
-- for each action in the program. However, new and continuing discoveries
-- regarding 'Scoped' has allowed the improvement of having the interpreter be
-- used only once per use of 'scoped', and have it cover the same scope of
-- actions that @withResource@ does.
--
-- This renders @withResource@ practically redundant; for the moment, the API
-- surrounding 'Scoped' remains the same, but work is in progress to revamp the
-- entire API of 'Scoped'.
runScoped ::
  ∀ resource param effect r .
  (∀ x . param -> (resource -> Sem r x) -> Sem r x) ->
  (resource -> InterpreterFor effect r) ->
  InterpreterFor (Scoped param effect) r
runScoped withResource scopedInterpreter =
  interpretWeaving \(Weaving effect s wv ex _) -> case effect of
    Run _ -> errorWithoutStackTrace "top level run"
    InScope param main -> withResource param \ resource ->
      ex <$> scopedInterpreter resource (go (raiseUnder $ wv (main <$ s)))
  where
    go :: InterpreterFor (Scoped param effect) (effect : r)
    go =
      interpretWeaving \ (Weaving effect s wv ex ins) -> case effect of
        Run act -> liftSem $ injWeaving $ Weaving act s (go . wv) ex ins
        InScope param main ->
          raise $ withResource param \ resource ->
            ex <$> scopedInterpreter resource (go (wv (main <$ s)))
{-# inline runScoped #-}

-- | Variant of 'runScoped' in which the resource allocator returns the resource
-- rather tnen calling a continuation.
runScopedAs ::
  ∀ resource param effect r .
  (param -> Sem r resource) ->
  (resource -> InterpreterFor effect r) ->
  InterpreterFor (Scoped param effect) r
runScopedAs resource = runScoped \ p use -> use =<< resource p
{-# inline runScopedAs #-}

-- | Combined higher-order interpreter for 'Scoped' and 'Resumable'.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
interpretScopedResumableH ::
  ∀ param resource effect err r .
  (∀ x . param -> (resource -> Sem (Stop err : r) x) -> Sem (Stop err : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) (Stop err : r) x) ->
  InterpreterFor (Scoped param effect !! err) r
interpretScopedResumableH withResource scopedHandler =
  interpretWeaving \case
    Weaving (Resumable (Weaving (InScope param main) s' wv' ex' _)) s wv ex _ -> do
      exResumable s ex ex' $ runStop $ withResource param \ resource ->
        interpretH (scopedHandler resource) (raiseUnder (go $ raiseUnder $ wv ((wv' (main <$ s')) <$ s)))
    _ ->
      errorWithoutStackTrace "top level run"
  where
    go :: InterpreterFor (Scoped param effect !! err) (effect : r)
    go =
      interpretWeaving \ (Weaving (Resumable (Weaving effect s' wv' ex' ins')) s wv ex ins) -> case effect of
        Run act ->
          ex . fmap Right <$> liftSem (weave s (go . wv) ins (injWeaving (Weaving act s' wv' ex' ins')))
        InScope param main -> do
          exResumable s ex ex' $ raise $ runStop $ withResource param \ resource ->
            interpretH (scopedHandler resource) (raiseUnder (go $ wv (wv' (main <$ s') <$ s)))

-- | Combined interpreter for 'Scoped' and 'Resumable'.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
interpretScopedResumable ::
  ∀ param resource effect err r .
  (∀ x . param -> (resource -> Sem (Stop err : r) x) -> Sem (Stop err : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : r) x) ->
  InterpreterFor (Scoped param effect !! err) r
interpretScopedResumable withResource scopedHandler =
  interpretScopedResumableH withResource \ r e -> liftT (scopedHandler r e)

-- | Combined interpreter for 'Scoped' and 'Resumable'.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
-- In this variant, the resource allocator is a plain action.
interpretScopedResumable_ ::
  ∀ param resource effect err r .
  (param -> Sem (Stop err : r) resource) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : r) x) ->
  InterpreterFor (Scoped param effect !! err) r
interpretScopedResumable_ resource =
  interpretScopedResumable \ p use -> use =<< resource p

-- | Combined higher-order interpreter for 'Scoped' and 'Resumable' that allows the handler to use additional effects
-- that are interpreted by the resource allocator.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
interpretScopedResumableWithH ::
  ∀ extra param resource effect err r r1 extraerr .
  extraerr ~ (extra ++ '[Stop err]) =>
  r1 ~ (extraerr ++ r) =>
  KnownList (extra ++ '[Stop err]) =>
  (∀ x . param -> (resource -> Sem r1 x) -> Sem (Stop err : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) r1 x) ->
  InterpreterFor (Scoped param effect !! err) r
interpretScopedResumableWithH withResource scopedHandler =
  interpretWeaving \case
    Weaving (Resumable (Weaving (InScope param main) s' wv' ex' _)) s wv ex _ -> do
      exResumable s ex ex' $ runStop $ withResource param \ resource ->
        interpretH (scopedHandler resource) $ inScope $
          restack
            (injectMembership
             (singList @'[Scoped param effect !! err])
             (singList @(effect : extraerr))) $ wv (wv' (main <$ s') <$ s)
    _ ->
      errorWithoutStackTrace "top level Run"
  where
    inScope :: InterpreterFor (Scoped param effect !! err) (effect : r1)
    inScope =
      interpretWeaving \ (Weaving (Resumable (Weaving effect s' wv' ex' ins')) s wv ex ins) -> case effect of
        InScope param main ->
          restack
            (extendMembershipLeft (singList @(effect : extraerr))) $
            exResumable s ex ex' $ runStop $ withResource param \ resource ->
              interpretH (scopedHandler resource) (inScope $ wv (wv' (main <$ s') <$ s))
        Run act ->
          ex . fmap Right <$> liftSem (weave s (inScope . wv) ins (injWeaving (Weaving act s' wv' ex' ins')))

-- | Combined interpreter for 'Scoped' and 'Resumable' that allows the handler to use additional effects that are
-- interpreted by the resource allocator.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
interpretScopedResumableWith ::
  ∀ extra param resource effect err r r1 .
  r1 ~ ((extra ++ '[Stop err]) ++ r) =>
  KnownList (extra ++ '[Stop err]) =>
  (∀ x . param -> (resource -> Sem r1 x) -> Sem (Stop err : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem r1 x) ->
  InterpreterFor (Scoped param effect !! err) r
interpretScopedResumableWith withResource scopedHandler =
  interpretScopedResumableWithH @extra withResource \ r e -> liftT (scopedHandler r e)

-- | Combined interpreter for 'Scoped' and 'Resumable' that allows the handler to use additional effects that are
-- interpreted by the resource allocator.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
-- In this variant, no resource is used and the allocator is a plain interpreter.
interpretScopedResumableWith_ ::
  ∀ extra param effect err r r1 .
  r1 ~ ((extra ++ '[Stop err]) ++ r) =>
  KnownList (extra ++ '[Stop err]) =>
  (∀ x . param -> Sem r1 x -> Sem (Stop err : r) x) ->
  (∀ r0 x . effect (Sem r0) x -> Sem r1 x) ->
  InterpreterFor (Scoped param effect !! err) r
interpretScopedResumableWith_ extra scopedHandler =
  interpretScopedResumableWith @extra (\ p f -> extra p (f ())) (const scopedHandler)

-- | Combined higher-order interpreter for 'Resumable' and 'Scoped'.
-- In this variant, only the handler may send 'Stop', but this allows resumption to happen on each action inside of the
-- scope.
interpretResumableScopedH ::
  ∀ param resource effect err r .
  (∀ x . param -> (resource -> Sem r x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical (effect !! err) (Sem r0) (Stop err : r) x) ->
  InterpreterFor (Scoped param (effect !! err)) r
interpretResumableScopedH withResource scopedHandler =
  interpretWeaving $ \(Weaving effect s wv ex _) -> case effect of
    Run _ -> errorWithoutStackTrace "top level run"
    InScope param main -> withResource param \ resource ->
      ex <$> interpretResumableH (scopedHandler resource) (inScope $ raiseUnder $ wv (main <$ s))
  where
    inScope :: InterpreterFor (Scoped param (effect !! err)) (effect !! err : r)
    inScope =
      interpretWeaving \ (Weaving effect s wv ex ins) -> case effect of
        Run act -> liftSem $ injWeaving $ Weaving act s (inScope . wv) ex ins
        InScope param main -> raise $ withResource param \ resource ->
          ex <$> interpretResumableH (scopedHandler resource) (inScope $ wv (main <$ s))

-- | Combined interpreter for 'Resumable' and 'Scoped'.
-- In this variant, only the handler may send 'Stop', but this allows resumption to happen on each action inside of the
-- scope.
interpretResumableScoped ::
  ∀ param resource effect err r .
  (∀ x . param -> (resource -> Sem r x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : r) x) ->
  InterpreterFor (Scoped param (effect !! err)) r
interpretResumableScoped withResource scopedHandler =
  interpretResumableScopedH withResource \ r e -> liftT (scopedHandler r e)

-- | Combined interpreter for 'Resumable' and 'Scoped'.
-- In this variant:
-- - Only the handler may send 'Stop', but this allows resumption to happen on each action inside of the scope.
-- - The resource allocator is a plain action.
interpretResumableScoped_ ::
  ∀ param resource effect err r .
  (param -> Sem r resource) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : r) x) ->
  InterpreterFor (Scoped param (effect !! err)) r
interpretResumableScoped_ resource =
  interpretResumableScoped \ p use -> use =<< resource p

-- | Combined higher-order interpreter for 'Resumable' and 'Scoped' that allows the handler to use additional effects
-- that are interpreted by the resource allocator.
-- In this variant, only the handler may send 'Stop', but this allows resumption to happen on each action inside of the
-- scope.
interpretResumableScopedWithH ::
  ∀ extra param resource effect err r r1 .
  r1 ~ (extra ++ r) =>
  KnownList extra =>
  (∀ x . param -> (resource -> Sem r1 x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical (effect !! err) (Sem r0) (Stop err : r1) x) ->
  InterpreterFor (Scoped param (effect !! err)) r
interpretResumableScopedWithH withResource scopedHandler =
  interpretWeaving \case
    Weaving (InScope param main) s wv ex _ ->
      ex <$> withResource param \ resource ->
        interpretResumableH (scopedHandler resource) $ inScope $
          restack
            (injectMembership
             (singList @'[Scoped param (effect !! err)])
             (singList @(effect !! err : extra))) $ wv (main <$ s)
    _ ->
      errorWithoutStackTrace "top level Run"
  where
    inScope :: InterpreterFor (Scoped param (effect !! err)) (effect !! err : r1)
    inScope =
      interpretWeaving \case
        Weaving (InScope param main) s wv ex _ ->
          restack
            (extendMembershipLeft (singList @(effect !! err : extra)))
            (ex <$> withResource param \resource ->
                interpretResumableH (scopedHandler resource) $ inScope $ wv (main <$ s))
        Weaving (Run act) s wv ex ins ->
          liftSem $ injWeaving $ Weaving act s (inScope . wv) ex ins

-- | Combined interpreter for 'Resumable' and 'Scoped' that allows the handler to use additional effects that are
-- interpreted by the resource allocator.
-- In this variant, only the handler may send 'Stop', but this allows resumption to happen on each action inside of the
-- scope.
interpretResumableScopedWith ::
  ∀ extra param resource effect err r r1 .
  r1 ~ (extra ++ r) =>
  KnownList extra =>
  (∀ x . param -> (resource -> Sem r1 x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : r1) x) ->
  InterpreterFor (Scoped param (effect !! err)) r
interpretResumableScopedWith withResource scopedHandler =
  interpretResumableScopedWithH @extra withResource \ r e -> liftT (scopedHandler r e)

-- | Combined interpreter for 'Resumable' and 'Scoped' that allows the handler to use additional effects that are
-- interpreted by the resource allocator.
-- In this variant:
-- - Only the handler may send 'Stop', but this allows resumption to happen on each action inside of the scope.
-- - No resource is used and the allocator is a plain interpreter.
interpretResumableScopedWith_ ::
  ∀ extra param effect err r r1 .
  r1 ~ (extra ++ r) =>
  KnownList extra =>
  (∀ x . param -> Sem r1 x -> Sem r x) ->
  (∀ r0 x . effect (Sem r0) x -> Sem (Stop err : r1) x) ->
  InterpreterFor (Scoped param (effect !! err)) r
interpretResumableScopedWith_ extra scopedHandler =
  interpretResumableScopedWith @extra @param @() @effect @err @r @r1 (\ p f -> extra p (f ())) (const scopedHandler)

-- | Combined higher-order interpreter for 'Resumable' and 'Scoped'.
-- In this variant, both the handler and the scope may send different errors via 'Stop', encoding the concept that the
-- resource allocation may fail to prevent the scope from being executed, and each individual scoped action may fail,
-- continuing the scope execution on resumption.
interpretScopedRH ::
  ∀ param resource effect eo ei r .
  (∀ x . param -> (resource -> Sem (Stop eo : r) x) -> Sem (Stop eo : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical (effect !! ei) (Sem r0) (Stop ei : Stop eo : r) x) ->
  InterpreterFor (Scoped param (effect !! ei) !! eo) r
interpretScopedRH withResource scopedHandler =
  interpretWeaving \case
    Weaving (Resumable (Weaving (InScope param main) s' wv' ex' _)) s wv ex _ -> do
      exResumable s ex ex' $ runStop $ withResource param \ resource ->
        interpretResumableH (scopedHandler resource) (raiseUnder (inScope $ raiseUnder $ wv (wv' (main <$ s') <$ s)))
    _ ->
      errorWithoutStackTrace "top level run"
  where
    inScope :: InterpreterFor (Scoped param (effect !! ei) !! eo) (effect !! ei : r)
    inScope =
      interpretWeaving \ (Weaving (Resumable (Weaving effect s' wv' ex' ins')) s wv ex ins) -> case effect of
        Run act ->
          ex . fmap Right <$> liftSem (weave s (inScope . wv) ins (injWeaving (Weaving act s' wv' ex' ins')))
        InScope param main -> do
          exResumable s ex ex' $ raise $ runStop $ withResource param \ resource ->
            interpretResumableH (scopedHandler resource) (raiseUnder (inScope $ wv (wv' (main <$ s') <$ s)))

-- | Combined interpreter for 'Scoped' and 'Resumable'.
-- In this variant, both the handler and the scope may send different errors via 'Stop', encoding the concept that the
-- resource allocation may fail to prevent the scope from being executed, and each individual scoped action may fail,
-- continuing the scope execution on resumption.
interpretScopedR ::
  ∀ param resource effect eo ei r .
  (∀ x . param -> (resource -> Sem (Stop eo : r) x) -> Sem (Stop eo : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop ei : Stop eo : r) x) ->
  InterpreterFor (Scoped param (effect !! ei) !! eo) r
interpretScopedR withResource scopedHandler =
  interpretScopedRH withResource \ r e -> liftT (scopedHandler r e)

-- | Combined interpreter for 'Scoped' and 'Resumable'.
-- In this variant:
-- - Both the handler and the scope may send different errors via 'Stop', encoding the concept that the
--   resource allocation may fail to prevent the scope from being executed, and each individual scoped action may fail,
--   continuing the scope execution on resumption.
-- - The resource allocator is a plain action.
interpretScopedR_ ::
  ∀ param resource effect eo ei r .
  (param -> Sem (Stop eo : r) resource) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop ei : Stop eo : r) x) ->
  InterpreterFor (Scoped param (effect !! ei) !! eo) r
interpretScopedR_ resource =
  interpretScopedR \ p use -> use =<< resource p

-- | Combined higher-order interpreter for 'Scoped' and 'Resumable' that allows the handler to use additional effects
-- that are interpreted by the resource allocator.
-- In this variant, both the handler and the scope may send different errors via 'Stop', encoding the concept that the
-- resource allocation may fail to prevent the scope from being executed, and each individual scoped action may fail,
-- continuing the scope execution on resumption.
interpretScopedRWithH ::
  ∀ extra param resource effect eo ei r r1 extraerr .
  extraerr ~ (extra ++ '[Stop eo]) =>
  r1 ~ (extra ++ Stop eo : r) =>
  r1 ~ ((extra ++ '[Stop eo]) ++ r) =>
  KnownList (extra ++ '[Stop eo]) =>
  (∀ x . param -> (resource -> Sem (extra ++ Stop eo : r) x) -> Sem (Stop eo : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical (effect !! ei) (Sem r0) (Stop ei : r1) x) ->
  InterpreterFor (Scoped param (effect !! ei) !! eo) r
interpretScopedRWithH withResource scopedHandler =
  interpretWeaving \case
    Weaving (Resumable (Weaving (InScope param main) s' wv' ex' _)) s wv ex _ -> do
      exResumable s ex ex' $ runStop $ withResource param \ resource ->
        interpretResumableH (scopedHandler resource) $ inScope $
          restack
            (injectMembership
             (singList @'[Scoped param (effect !! ei) !! eo])
             (singList @(effect !! ei : extraerr))) $ wv (wv' (main <$ s') <$ s)
    _ ->
      errorWithoutStackTrace "top level Run"
  where
    inScope :: InterpreterFor (Scoped param (effect !! ei) !! eo) (effect !! ei : r1)
    inScope =
      interpretWeaving \ (Weaving (Resumable (Weaving effect s' wv' ex' ins')) s wv ex ins) -> case effect of
        InScope param main ->
          exResumable s ex ex' $
          restack (extendMembershipLeft (singList @(effect !! ei : extraerr))) $
          runStop $
          withResource param \ resource ->
            interpretResumableH (scopedHandler resource) (inScope $ wv (wv' (main <$ s') <$ s))
        Run act ->
          ex . fmap Right <$> liftSem (weave s (inScope . wv) ins (injWeaving (Weaving act s' wv' ex' ins')))

-- | Combined interpreter for 'Scoped' and 'Resumable' that allows the handler to use additional effects that are
-- interpreted by the resource allocator.
-- In this variant, both the handler and the scope may send different errors via 'Stop', encoding the concept that the
-- resource allocation may fail to prevent the scope from being executed, and each individual scoped action may fail,
-- continuing the scope execution on resumption.
interpretScopedRWith ::
  ∀ extra param resource effect eo ei r r1 .
  r1 ~ (extra ++ Stop eo : r) =>
  r1 ~ ((extra ++ '[Stop eo]) ++ r) =>
  KnownList (extra ++ '[Stop eo]) =>
  (∀ x . param -> (resource -> Sem r1 x) -> Sem (Stop eo : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop ei : r1) x) ->
  InterpreterFor (Scoped param (effect !! ei) !! eo) r
interpretScopedRWith withResource scopedHandler =
  interpretScopedRWithH @extra withResource \ r e -> liftT (scopedHandler r e)

-- | Combined interpreter for 'Scoped' and 'Resumable' that allows the handler to use additional effects that are
-- interpreted by the resource allocator.
-- - Both the handler and the scope may send different errors via 'Stop', encoding the concept that the
--   resource allocation may fail to prevent the scope from being executed, and each individual scoped action may fail,
--   continuing the scope execution on resumption.
-- - The resource allocator is a plain action.
interpretScopedRWith_ ::
  ∀ extra param effect eo ei r r1 .
  r1 ~ (extra ++ Stop eo : r) =>
  r1 ~ ((extra ++ '[Stop eo]) ++ r) =>
  KnownList (extra ++ '[Stop eo]) =>
  (∀ x . param -> Sem r1 x -> Sem (Stop eo : r) x) ->
  (∀ r0 x . effect (Sem r0) x -> Sem (Stop ei : r1) x) ->
  InterpreterFor (Scoped param (effect !! ei) !! eo) r
interpretScopedRWith_ extra scopedHandler =
  interpretScopedRWith @extra (\ p f -> extra p (f ())) (const scopedHandler)
