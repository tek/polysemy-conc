module Polysemy.Conc.Test.QueueTest where

import Polysemy.Async (asyncToIOFinal, Async)
import Polysemy.Test (UnitTest, assertEq, assertJust, runTestAuto)
import Polysemy.Time (MilliSeconds(MilliSeconds), interpretTimeGhc)
import Polysemy.Time.Ghc (GhcTime)

import qualified Polysemy.Conc.Data.Queue as Queue
import Polysemy.Conc.Data.Queue (Queue)
import qualified Polysemy.Conc.Data.QueueResult as QueueResult
import Polysemy.Conc.Data.Race (Race)
import Polysemy.Conc.Queue.Result (resultToMaybe)
import Polysemy.Conc.Queue.TB (interpretQueueTB)
import Polysemy.Conc.Queue.TBM (interpretQueueTBM)
import Polysemy.Conc.Race (interpretRace)
import Polysemy.Conc.Data.QueueResult (QueueResult)

progSuccess ::
  Member (Queue Int) r =>
  Sem r (Maybe Int)
progSuccess = do
  Queue.write @Int 5
  resultToMaybe <$> Queue.read

progTimeoutSuccess ::
  Member (Queue Int) r =>
  Sem r (Maybe Int)
progTimeoutSuccess = do
  Queue.write @Int 5
  resultToMaybe <$> Queue.readTimeout (MilliSeconds 100)

progPeekSuccess ::
  Member (Queue Int) r =>
  Sem r (QueueResult Int, QueueResult Int, Maybe Int)
progPeekSuccess = do
  r1 <- Queue.tryPeek @Int
  Queue.write @Int 5
  r2 <- Queue.peek @Int
  r3 <- resultToMaybe <$> Queue.read @Int
  pure (r1, r2, r3)

run ::
  Members [Embed IO, Final IO] r =>
  Sem (Race : GhcTime : Async : r) a ->
  Sem r a
run =
  asyncToIOFinal .
  interpretTimeGhc .
  interpretRace

test_queueTBM :: UnitTest
test_queueTBM =
  runTestAuto do
    result <- run $ interpretQueueTBM @Int 1 $ progSuccess
    assertJust @_ @IO 5 result

test_queueTimeoutTBM :: UnitTest
test_queueTimeoutTBM =
  runTestAuto do
    result <- run $ interpretQueueTBM @Int 1 $ progTimeoutSuccess
    assertJust @_ @IO 5 result

test_queuePeekTBM :: UnitTest
test_queuePeekTBM =
  runTestAuto do
    result <- run $ interpretQueueTBM @Int 1 $ progPeekSuccess
    assertEq @_ @IO (QueueResult.NotAvailable, QueueResult.Success 5, Just 5) result

test_queueBlockTBM :: UnitTest
test_queueBlockTBM =
  runTestAuto do
    result <- run $ interpretQueueTBM @() 1 $ Queue.readTimeout @() (MilliSeconds 100)
    assertEq @_ @IO QueueResult.NotAvailable result

test_queueTB :: UnitTest
test_queueTB =
  runTestAuto do
    result <- run $ interpretQueueTB @Int 1 $ progSuccess
    assertJust @_ @IO 5 result

test_queueBlockTB :: UnitTest
test_queueBlockTB =
  runTestAuto do
    result <- run $ interpretQueueTB @() 1 $ Queue.readTimeout @() (MilliSeconds 100)
    assertEq @_ @IO QueueResult.NotAvailable result
