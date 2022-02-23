module Polysemy.Conc.Test.InterruptTest where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVarIO, readTVarIO)
import Polysemy.Test (UnitTest, assertEq, runTestAuto)
import System.Posix (Handler (CatchInfoOnce), SignalInfo, installHandler, keyboardSignal, raiseSignal)

import qualified Polysemy.Conc.Effect.Interrupt as Interrupt
import Polysemy.Conc.Interpreter.Critical (interpretCritical)
import Polysemy.Conc.Interpreter.Interrupt (interpretInterrupt)
import Polysemy.Conc.Interpreter.Race (interpretRace)

handler :: MVar () -> TVar Int -> SignalInfo -> IO ()
handler mv tv _ = do
  atomically (modifyTVar tv (5 +))
  putMVar mv ()

test_interrupt :: UnitTest
test_interrupt = do
  runTestAuto do
    tv <- embed (newTVarIO 0)
    mv <- embed newEmptyMVar
    embed (installHandler keyboardSignal (CatchInfoOnce (handler mv tv)) Nothing)
    asyncToIOFinal $ interpretCritical $ interpretRace $ interpretInterrupt do
      Interrupt.register "test 1" do
        atomically (modifyTVar tv (3 +))
      Interrupt.register "test 2" do
        atomically (modifyTVar tv (9 +))
      embed (raiseSignal keyboardSignal)
    embed (takeMVar mv)
    result <- embed (readTVarIO tv)
    assertEq @_ @IO 17 result
