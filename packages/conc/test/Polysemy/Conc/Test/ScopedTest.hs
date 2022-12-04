{-# options_ghc -fplugin=Polysemy.Plugin #-}

module Polysemy.Conc.Test.ScopedTest where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO, writeTVar)
import Polysemy.Resume (Stop, stop, (!!), resumeAs, resuming, type (!!), interpretResumableH)
import Polysemy.Test (UnitTest, runTestAuto, unitTest, (===))
import Test.Tasty (TestTree, testGroup)

import Polysemy.Conc.Effect.Scoped (rescope, scoped, scoped_, Scoped)
import Polysemy.Conc.Interpreter.Scoped (
  interpretScoped,
  interpretScopedH,
  interpretScopedH',
  interpretScopedResumableWithH,
  interpretScopedWithH, interpretScopedResumableH, interpretResumableScopedH, interpretResumableScopedWithH, interpretScopedRH, interpretScopedRWithH,
  )

newtype Par =
  Par { unPar :: Int }
  deriving stock (Eq, Show)
  deriving newtype (Num, Real, Enum, Integral, Ord)

data E :: Effect where
  E1 :: E m Int
  E2 :: E m Int

makeSem ''E

data F :: Effect where
  F :: F m Int

makeSem ''F

handleE ::
  Member (Embed IO) r =>
  TVar Int ->
  E m a ->
  Tactical effect m (F : r) a
handleE tv = \case
  E1 -> do
    i1 <- embed (readTVarIO tv)
    i2 <- f
    pureT (i1 + i2 + 10)
  E2 ->
    pureT (-1)

interpretF ::
  Member (Embed IO) r =>
  TVar Int ->
  InterpreterFor F r
interpretF tv =
  interpret \ F -> do
    embed (atomically (writeTVar tv 7))
    pure 5

scope ::
  Member (Embed IO) r =>
  Par ->
  (TVar Int -> Sem (F : r) a) ->
  Sem r a
scope (Par n) use = do
  tv <- embed (newTVarIO n)
  interpretF tv (use tv)

test_scopedWith :: UnitTest
test_scopedWith =
  runTestAuto $ interpretScopedWithH @'[F] @_ @_ @E scope handleE do
    (i1, i2) <- scoped @_ @E 20 do
      i1 <- e1
      i2 <- scoped @_ @E 23 e1
      pure (i1, i2)
    35 === i1
    38 === i2

data H1 :: Effect where
  H1a :: m Int -> H1 m Int
  H1b :: H1 m Int
  H1c :: H1 m Int
  H1d :: m Int -> H1 m Int

makeSem ''H1

scopeH1 :: Int -> (Int -> Sem (Stop Int : r) a) -> Sem (Stop Int : r) a
scopeH1 p use =
  if p == (-1) then stop 500000 else use (p + 5)

handleH1 :: Int -> H1 m a -> Tactical H1 m (Stop Int : r) a
handleH1 n = \case
  H1a ma -> do
    i <- runTSimple ma
    pure (i <&> \ i' -> i' + n + 1)
  H1b ->
    pureT 50000
  H1c ->
    stop 100000
  H1d ma ->
    raise . interpretH (handleH1 (n + 1)) =<< runT ma

test_scopedResumable :: UnitTest
test_scopedResumable =
  runTestAuto $ interpretScopedResumableH @Int @Int @H1 @Int scopeH1 handleH1 do
    (i1, i2, i3, i4, i5) <- resumeAs @Int @(Scoped Int H1) (-50, -100, -200, -500, -700) $ scoped @Int @H1 20 do
      i1 <- h1a do
        h1b
      i2 <- resumeAs @Int @(Scoped Int H1) (-1000) $ scoped @Int @H1 23 do
        h1a do
          h1b
      i3 <- resuming pure $ scoped @Int @H1 5000 do
        h1a do
          h1c
      i4 <- resuming pure $ scoped @Int @H1 (-1) do
        h1b
      i5 <- h1d do
        h1a h1b
      pure (i1, i2, i3, i4, i5)
    50026 === i1
    50029 === i2
    100000 === i3
    500000 === i4
    50027 === i5

handleRE ::
  Member (Embed IO) r =>
  TVar Int ->
  E m a ->
  Tactical effect m (F : Stop Int : r) a
handleRE tv = \case
  E1 -> do
    i1 <- embed (readTVarIO tv)
    i2 <- f
    _ <- stop (i1 + i2)
    pureT (i1 + i2 + 10)
  E2 ->
    pureT =<< f

scopeR ::
  Member (Embed IO) r =>
  Par ->
  (TVar Int -> Sem (F : Stop Int : r) a) ->
  Sem (Stop Int : r) a
scopeR (Par n) use = do
  tv <- embed (newTVarIO n)
  _ <- interpretF tv (use tv)
  stop . (50 +) =<< embed (readTVarIO tv)

test_scopedResumableWith :: UnitTest
test_scopedResumableWith =
  runTestAuto $ interpretScopedResumableWithH @'[F] scopeR handleRE do
    i1 <- scoped 20 e1 !! pure
    i2 <- scoped 23 e2 !! pure
    25 === i1
    57 === i2

data Rs :: Effect where
  Rs1 :: m Int -> Rs m Int
  Rs2 :: Rs m Int
  Rs3 :: m Int -> Rs m Int
  Rs4 :: Rs m Int

makeSem ''Rs

scopeRs :: Int -> (Int -> Sem r a) -> Sem r a
scopeRs n use =
  use (n + 1)

handleRs :: Int -> Rs m a -> Tactical (Rs !! Int) m (Stop Int : r) a
handleRs n = \case
  Rs1 ma -> raise . interpretResumableH (handleRs (n + 1000000)) =<< runT ma
  Rs2 -> pureT (n + 20)
  Rs3 _ -> stop 200
  Rs4 -> stop 2000

test_resumableScoped :: UnitTest
test_resumableScoped =
  runTestAuto $ interpretResumableScopedH @Int @Int @Rs @Int scopeRs handleRs do
    (i1, i2, i3, i4, i5) <- scoped 10000 do
      i1 <- resuming pure $ rs1 do
        rs2
      i2 <- resuming pure $ rs3 do
        rs4
      i3 <- rs2 !! pure
      i4 <- resuming pure $ rs1 do
        rs4
      i5 <- raise $ scoped @Int @(Rs !! Int) 100000 do
        resumeAs 1000 $ rs1 rs2
      pure (i1, i2, i3, i4, i5)
    1010021 === i1
    200 === i2
    10021 === i3
    2000 === i4
    1100021 === i5

data Rsw :: Effect where
  Rsw1 :: m Int -> Rsw m Int
  Rsw2 :: Rsw m Int
  Rsw3 :: m Int -> Rsw m Int
  Rsw4 :: Rsw m Int

makeSem ''Rsw

data RswExtra :: Effect where
  RswExtra :: RswExtra m Int

makeSem ''RswExtra

scopeRsw :: Int -> (Int -> Sem (RswExtra : r) a) -> Sem r a
scopeRsw n use =
  interpret (\ RswExtra -> pure 20) $
  use (n + 1)

handleRsw ::
  Member RswExtra r =>
  Int ->
  Rsw m a ->
  Tactical (Rsw !! Int) m (Stop Int : r) a
handleRsw n = \case
  Rsw1 ma -> raise . interpretResumableH (handleRsw (n + 1000000)) =<< runT ma
  Rsw2 -> pureT . (n +) =<< rswExtra
  Rsw3 _ -> stop 200
  Rsw4 -> stop 2000

test_resumableScopedWith :: UnitTest
test_resumableScopedWith =
  runTestAuto $ interpretResumableScopedWithH @'[RswExtra] scopeRsw handleRsw do
    (i1, i2, i3, i4, i5) <- scoped 10000 do
      i1 <- resuming pure $ rsw1 do
        rsw2
      i2 <- resuming pure $ rsw3 do
        rsw4
      i3 <- rsw2 !! pure
      i4 <- resuming pure $ rsw1 do
        rsw4
      i5 <- raise $ scoped @Int @(Rsw !! Int) 100000 do
        resumeAs 1000 $ rsw1 rsw2
      pure (i1, i2, i3, i4, i5)
    1010021 === i1
    200 === i2
    10021 === i3
    2000 === i4
    1100021 === i5

data Rsr :: Effect where
  Rsr1 :: m Int -> Rsr m Int
  Rsr2 :: Rsr m Int
  Rsr3 :: m Int -> Rsr m Int
  Rsr4 :: Rsr m Int

makeSem ''Rsr

scopeRsr :: Int -> (Int -> Sem (Stop Double : r) a) -> Sem (Stop Double : r) a
scopeRsr n use =
  use (n + 1)

handleRsr :: Int -> Rsr m a -> Tactical (Rsr !! Int) m (Stop Int : r) a
handleRsr n = \case
  Rsr1 ma -> raise . interpretResumableH (handleRsr (n + 1000000)) =<< runT ma
  Rsr2 -> pureT (n + 20)
  Rsr3 _ -> stop 200
  Rsr4 -> stop 2000

test_scopedR :: UnitTest
test_scopedR =
  runTestAuto $ interpretScopedRH @Int @Int @Rsr @Double @Int scopeRsr handleRsr do
    (i1, i2, i3, i4, i5) <- resuming (\ i -> pure (round i, round i, round i, round i, round i)) $ scoped 10000 do
      i1 <- resuming pure $ rsr1 do
        rsr2
      i2 <- resuming pure $ rsr3 do
        rsr4
      i3 <- rsr2 !! pure
      i4 <- resuming pure $ rsr1 do
        rsr4
      i5 <- raise $ scoped @Int @(Rsr !! Int) 100000 do
        resumeAs 1000 $ rsr1 rsr2
      pure (i1, i2, i3, i4, i5)
    1010021 === i1
    200 === i2
    10021 === i3
    2000 === i4
    1100021 === i5

scopeRsrw :: Int -> (Int -> Sem (RswExtra : Stop Double : r) a) -> Sem (Stop Double : r) a
scopeRsrw n use =
  interpret (\ RswExtra -> pure 20) $
  use (n + 1)

handleRsrw :: Int -> Rsr m a -> Tactical (Rsr !! Int) m (Stop Int : r) a
handleRsrw n = \case
  Rsr1 ma -> raise . interpretResumableH (handleRsr (n + 1000000)) =<< runT ma
  Rsr2 -> pureT (n + 20)
  Rsr3 _ -> stop 200
  Rsr4 -> stop 2000

test_scopedRWith :: UnitTest
test_scopedRWith =
  runTestAuto $ interpretScopedRWithH @'[RswExtra] scopeRsrw handleRsrw do
    (i1, i2, i3, i4, i5) <- resuming (\ i -> pure (round i, round i, round i, round i, round i)) $ scoped 10000 do
      i1 <- resuming pure $ rsr1 do
        rsr2
      i2 <- resuming pure $ rsr3 do
        rsr4
      i3 <- rsr2 !! pure
      i4 <- resuming pure $ rsr1 do
        rsr4
      i5 <- raise $ scoped @Int @(Rsr !! Int) 100000 do
        resumeAs 1000 $ rsr1 rsr2
      pure (i1, i2, i3, i4, i5)
    1010021 === i1
    200 === i2
    10021 === i3
    2000 === i4
    1100021 === i5

scopeH ::
  Member (Embed IO) r =>
  Par ->
  (TVar Int -> Tactical e m r a) ->
  Tactical e m r a
scopeH (Par n) use = do
  tv <- embed (newTVarIO n)
  use tv

handleH ::
  Member (Embed IO) r =>
  TVar Int ->
  E (Sem r0) a ->
  Tactical e (Sem r0) r a
handleH tv = \case
  E1 -> do
    embed (atomically (modifyTVar' tv (+1)))
    pureT =<< embed (readTVarIO tv)
  E2 ->
    pureT 23

test_scopedH' :: UnitTest
test_scopedH' =
  runTestAuto $ interpretScopedH' scopeH handleH do
    r <- scoped @_ @E 100 do
      i1 <- e1
      i2 <- scoped @_ @E 200 e1
      i3 <- e1
      pure (i1, i2, i3)
    (101, 201, 102) === r

data RPar =
  RPar {
    rpD :: Double,
    rpB :: Bool
  }
  deriving stock (Eq, Show)

data RRes =
  RRes {
    rrD :: Double,
    rrI :: Int
  }
  deriving stock (Eq, Show)

scopeR1 :: RPar -> (RRes -> Sem r a) -> Sem r a
scopeR1 (RPar d b) use =
  use (RRes (d * 2) (if b then 1 else 2))

handleR1 :: RRes -> E m x -> Sem r x
handleR1 (RRes d i) = \case
  E1 -> pure (i + floor (d * 3))
  E2 -> pure (i + floor (d * 4))

rescopeP :: Int -> RPar
rescopeP i =
  RPar (fromIntegral i) False

test_rescope :: UnitTest
test_rescope =
  runTestAuto $ interpretScoped scopeR1 handleR1 do
    r <- rescope rescopeP do
      scoped @_ @E 100 do
        e1
    602 === r

data HO :: Effect where
  Inc :: m a -> HO m a
  Ret :: HO m Int

makeSem ''HO

scopeHO :: () -> (() -> Sem r a) -> Sem r a
scopeHO () use =
  use ()

handleHO :: Int -> () -> HO m a -> Tactical HO m r a
handleHO n () = \case
  Inc ma -> raise . interpretH (handleHO (n + 1) ()) =<< runT ma
  Ret -> pureT n

test_higherOrder :: UnitTest
test_higherOrder =
  runTestAuto $ interpretScopedH scopeHO (handleHO 1) do
    r <- scoped_ @HO do
      inc do
        ret
    2 === r

data Esc :: Effect where
  Esc :: Esc m Int
makeSem ''Esc

data Indirect :: Effect where
  Indirect :: Indirect m Int
makeSem ''Indirect

interpretIndirect :: Member Esc r => InterpreterFor Indirect r
interpretIndirect = interpret \ Indirect -> esc

handleEsc :: Int -> Esc m a -> Sem r a
handleEsc i = \ Esc -> pure i

test_escape :: UnitTest
test_escape =
  runTestAuto $ interpretScoped (flip ($)) handleEsc $ scoped @_ @Esc 2 $ interpretIndirect do
    r <- scoped @_ @Esc 1 indirect
    2 === r

test_scoped :: TestTree
test_scoped =
  testGroup "scoped" [
    unitTest "scopedWith" test_scopedWith,
    unitTest "scopedResumable" test_scopedResumable,
    unitTest "scopedResumableWith" test_scopedResumableWith,
    unitTest "resumableScoped" test_resumableScoped,
    unitTest "resumableScopedWith" test_resumableScopedWith,
    unitTest "scopedR" test_scopedR,
    unitTest "scopedRWith" test_scopedRWith,
    unitTest "scopedH'" test_scopedH',
    unitTest "rescope" test_rescope,
    unitTest "switch higher-order interpreter" test_higherOrder
    -- unitTest "nested scope with other interpreter in between" test_escape
  ]
