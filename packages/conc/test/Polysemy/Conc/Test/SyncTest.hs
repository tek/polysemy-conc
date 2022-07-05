module Polysemy.Conc.Test.SyncTest where

import Data.Time (Day, UTCTime)
import Polysemy.Test (UnitTest, assertEq, runTestAuto)
import qualified Polysemy.Time as Time
import Polysemy.Time (MicroSeconds (MicroSeconds), interpretTimeGhc)

import Polysemy.Conc.AtomicState (interpretAtomic)
import Polysemy.Conc.Effect.Race (Race)
import qualified Polysemy.Conc.Effect.Sync as Sync
import Polysemy.Conc.Effect.Sync (Sync)
import Polysemy.Conc.Interpreter.Race (interpretRace)
import Polysemy.Conc.Interpreter.Sync (interpretSync, interpretSyncAs)
import Polysemy.Conc.Sync (lock)

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

test_syncLock :: UnitTest
test_syncLock =
  runTestAuto $
  interpretTimeGhc $
  asyncToIOFinal $
  interpretRace $
  interpretSyncAs () $
  interpretAtomic (0 :: Int) do
    void $ sequenceConcurrently @[] $ [(1 :: Int)..100] $> do
      lock () do
        cur <- atomicGet @Int
        Time.sleep @UTCTime @Day (MicroSeconds 100)
        atomicPut (cur + 1)
    assertEq @Int @IO 100 =<< atomicGet
