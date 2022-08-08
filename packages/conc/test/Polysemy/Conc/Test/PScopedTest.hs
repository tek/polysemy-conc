{-# options_ghc -fplugin=Polysemy.Plugin #-}

module Polysemy.Conc.Test.PScopedTest where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Polysemy.Resume (Stop, stop, (!!))
import Polysemy.Test (UnitTest, runTestAuto, (===))

import Polysemy.Conc.Effect.PScoped (pscoped)
import Polysemy.Conc.Interpreter.PScoped (interpretPScopedResumableWithH, interpretPScopedWithH)

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

test_pscopedWith :: UnitTest
test_pscopedWith =
  runTestAuto $ interpretPScopedWithH @'[F] scope handleE do
    i1 <- pscoped 20 e1
    i2 <- pscoped 23 e1
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

test_pscopedResumableWith :: UnitTest
test_pscopedResumableWith =
  runTestAuto $ interpretPScopedResumableWithH @'[F] scopeR handleRE do
    i1 <- pscoped 20 e1 !! pure
    i2 <- pscoped 23 e2 !! pure
    25 === i1
    57 === i2
