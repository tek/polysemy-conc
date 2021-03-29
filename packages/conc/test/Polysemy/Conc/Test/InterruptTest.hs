module Polysemy.Conc.Test.InterruptTest where

import Polysemy.Async (asyncToIOFinal)
import Polysemy.Test (UnitTest, runTestAuto, assertEq)
import System.Posix (Handler (CatchInfoOnce), SignalInfo, installHandler, keyboardSignal, raiseSignal)

import Polysemy.Conc.Critical (interpretCritical)
import qualified Polysemy.Conc.Data.Interrupt as Interrupt
import Polysemy.Conc.Interrupt (interpretInterrupt)
import Polysemy.Conc.Race (interpretRace)
import Control.Concurrent.STM (modifyTVar)

handler :: MVar () -> TVar Int -> SignalInfo -> IO ()
handler mv tv _ = do
  atomically (modifyTVar tv (5 +))
  putMVar mv ()

test_interrupt :: UnitTest
test_interrupt = do
  runTestAuto do
    tv <- newTVarIO 0
    mv <- newEmptyMVar
    embed (installHandler keyboardSignal (CatchInfoOnce (handler mv tv)) Nothing)
    asyncToIOFinal $ interpretCritical $ interpretRace $ interpretInterrupt do
      Interrupt.register "test 1" do
        atomically (modifyTVar tv (3 +))
      Interrupt.register "test 2" do
        atomically (modifyTVar tv (9 +))
      embed (raiseSignal keyboardSignal)
    takeMVar mv
    result <- readTVarIO tv
    assertEq @_ @IO 17 result
