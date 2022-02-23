{-# options_ghc -fplugin=Polysemy.Plugin #-}

module Polysemy.Conc.Test.ScopedTest where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Polysemy.Resume (Stop, stop, (!!))
import Polysemy.Test (UnitTest, runTestAuto, (===))

import Polysemy.Conc.Effect.Scoped (scoped)
import Polysemy.Conc.Interpreter.Scoped (interpretScopedResumableWithH, interpretScopedWithH)

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
  (TVar Int -> Sem (F : r) a) ->
  Sem r a
scope use = do
  tv <- embed (newTVarIO 20)
  interpretF tv (use tv)

test_scopedWith :: UnitTest
test_scopedWith =
  runTestAuto $ interpretScopedWithH @'[F] scope handleE do
    i1 <- scoped e1
    i2 <- scoped e1
    i1 === 35
    i2 === 35

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

interpretFR ::
  Members [Stop Int, Embed IO] r =>
  TVar Int ->
  InterpreterFor F r
interpretFR tv =
  interpret \ F -> do
    embed (atomically (writeTVar tv 7))
    pure 5

scopeR ::
  Member (Embed IO) r =>
  (TVar Int -> Sem (F : Stop Int : r) a) ->
  Sem (Stop Int : r) a
scopeR use = do
  tv <- embed (newTVarIO 20)
  _ <- interpretF tv (use tv)
  stop . (50 +) =<< embed (readTVarIO tv)

test_scopedResumableWith :: UnitTest
test_scopedResumableWith =
  runTestAuto $ interpretScopedResumableWithH @'[F] scopeR handleRE do
    i1 <- scoped e1 !! pure
    i2 <- scoped e2 !! pure
    i1 === 25
    i2 === 57
