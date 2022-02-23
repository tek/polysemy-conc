module Polysemy.Conc.Test.LockTest where

import Data.Time (Day, UTCTime)
import Polysemy.Test (UnitTest, assertEq, runTestAuto)
import qualified Polysemy.Time as Time
import Polysemy.Time (MicroSeconds (MicroSeconds), interpretTimeGhc)

import Polysemy.Conc.AtomicState (interpretAtomic)
import Polysemy.Conc.Interpreter.Race (interpretRace)
import Polysemy.Conc.Interpreter.Sync (interpretSyncAs)
import Polysemy.Conc.Sync (lock)

test_lock :: UnitTest
test_lock =
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
