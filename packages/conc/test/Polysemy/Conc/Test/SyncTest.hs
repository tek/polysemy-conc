module Polysemy.Conc.Test.SyncTest where

import Polysemy.Async (Async, asyncToIOFinal, sequenceConcurrently)
import Polysemy.Test (UnitTest, assertEq, runTestAuto)

import Polysemy.Conc.Effect.Race (Race)
import qualified Polysemy.Conc.Effect.Sync as Sync
import Polysemy.Conc.Effect.Sync (Sync)
import Polysemy.Conc.Interpreter.Race (interpretRace)
import Polysemy.Conc.Interpreter.Sync (interpretSync)

data Thread1 = Thread1
data Thread2 = Thread2

thread1 ::
  Members [Sync Int, Sync Thread1, Sync Thread2] r =>
  Sem r Int
thread1 = do
  a <- Sync.takeBlock @Int
  Sync.putBlock (a + 1)
  Sync.putBlock Thread2
  Sync.takeBlock @Thread1
  Sync.takeBlock

thread2 ::
  Members [Sync Int, Sync Thread1, Sync Thread2] r =>
  Sem r Int
thread2 = do
  Sync.takeTry @Int >>= \case
    Just a -> pure a
    Nothing -> do
      Sync.putBlock @Int 1
      Sync.takeBlock @Thread2
      a <- Sync.takeBlock
      Sync.putBlock (a + 1)
      Sync.putBlock Thread1
      pure a

run ::
  Members [Embed IO, Final IO] r =>
  Sem (Sync Int : Sync Thread2 : Sync Thread1 : Race : Async : r) a ->
  Sem r a
run =
  asyncToIOFinal .
  interpretRace .
  interpretSync @Thread1 .
  interpretSync @Thread2 .
  interpretSync @Int

test_sync :: UnitTest
test_sync =
  runTestAuto do
    result <- run $ sequenceConcurrently @[] [thread1, thread2]
    assertEq @_ @IO [Just 3, Just 2] result
