module Polysemy.Conc.Test.SyncTest where

import Polysemy.Async (Async, asyncToIOFinal, sequenceConcurrently)
import Polysemy.Test (UnitTest, assertEq, runTestAuto)

import Polysemy.Conc.Data.Race (Race)
import qualified Polysemy.Conc.Data.Sync as Sync
import Polysemy.Conc.Data.Sync (Sync)
import Polysemy.Conc.Race (interpretRace)
import Polysemy.Conc.Sync (interpretSync)

thread1 ::
  Members [Sync Int, Sync Text] r =>
  Sem r Int
thread1 = do
  Sync.putBlock @Text "a"
  a <- Sync.takeBlock @Int
  Sync.putBlock (a + 1)
  Sync.takeBlock

thread2 ::
  Members [Sync Int, Sync Text] r =>
  Sem r Int
thread2 = do
  _ <- Sync.takeBlock @Text
  Sync.takeTry @Int >>= \case
    Just a -> pure a
    Nothing -> do
      Sync.putBlock @Int 1
      a <- Sync.takeBlock
      Sync.putBlock (a + 1)
      pure a

run ::
  Members [Embed IO, Final IO] r =>
  Sem (Race : Async : r) a ->
  Sem r a
run =
  asyncToIOFinal .
  interpretRace

test_sync :: UnitTest
test_sync =
  runTestAuto do
    result <- run $ interpretSync @Text $ interpretSync @Int $ sequenceConcurrently @[] [thread1, thread2]
    assertEq @_ @IO [Just 3, Just 2] result
