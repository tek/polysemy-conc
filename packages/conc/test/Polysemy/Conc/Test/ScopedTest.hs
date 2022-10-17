{-# options_ghc -fplugin=Polysemy.Plugin #-}

module Polysemy.Conc.Test.ScopedTest where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO, writeTVar)
import Polysemy.Resume (Stop, stop, (!!))
import Polysemy.Test (UnitTest, runTestAuto, unitTest, (===))
import Test.Tasty (TestTree, testGroup)

import Polysemy.Conc.Effect.Scoped (rescope, scoped)
import Polysemy.Conc.Interpreter.Scoped (interpretScoped, interpretScopedH', interpretScopedResumableWithH, interpretScopedWithH)

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

test_scopedH :: UnitTest
test_scopedH =
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


test_scoped :: TestTree
test_scoped =
  testGroup "scoped" [
    unitTest "scopedWith" test_scopedWith,
    unitTest "scopedResumableWith" test_scopedResumableWith
  ]
