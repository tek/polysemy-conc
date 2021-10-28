{-# options_ghc -fplugin=Polysemy.Plugin #-}

module Polysemy.Conc.Test.MaskTest where

import qualified Control.Concurrent.Async as Base
import Control.Concurrent.Async (asyncThreadId)
import Control.Exception (throwTo)
import Polysemy (embedFinal)
import Polysemy.Async (Async, async, asyncToIOFinal, await)
import Polysemy.Error (fromExceptionSem, runError)
import Polysemy.Test (Hedgehog, UnitTest, assertEq, evalLeft, runTestAuto)
import System.IO.Error (userError)

import Polysemy.Conc.AtomicState (interpretAtomic)
import Polysemy.Conc.Effect.Mask (restore, uninterruptibleMask)
import qualified Polysemy.Conc.Effect.Sync as Sync
import Polysemy.Conc.Interpreter.Mask (interpretUninterruptibleMaskFinal)
import Polysemy.Conc.Interpreter.Race (interpretRace)
import Polysemy.Conc.Interpreter.Sync (interpretSync)
import qualified Polysemy.Time as Time
import Polysemy.Time (interpretTimeGhc, MilliSeconds (MilliSeconds))

kill ::
  Member (Final IO) r =>
  Base.Async (Maybe ()) ->
  Sem r ()
kill t =
  embedFinal (throwTo (asyncThreadId t) (userError "stop"))

wait ::
  HasCallStack =>
  Members [Hedgehog IO, Async, Final IO] r =>
  Base.Async (Maybe ()) ->
  Sem r ()
wait t =
  withFrozenCallStack do
    void . evalLeft =<< runError @SomeException (fromExceptionSem @SomeException (await t))

test_mask :: UnitTest
test_mask =
  runTestAuto $
  asyncToIOFinal $
  interpretRace $
  interpretTimeGhc $
  interpretUninterruptibleMaskFinal $
  interpretAtomic (0 :: Int) $
  interpretSync @(Proxy 2) $
  interpretSync @(Proxy 1) do
    t1 <- async do
      uninterruptibleMask do
        Sync.putBlock (Proxy @1)
        Sync.takeBlock @(Proxy 2)
        Time.sleep (MilliSeconds 100)
        atomicPut 2
    Sync.takeBlock @(Proxy 1)
    async (kill t1)
    Sync.putBlock (Proxy @2)
    _ <- wait t1
    assertEq 2 =<< atomicGet
    t2 <- async do
      uninterruptibleMask do
        restore do
          Sync.putBlock (Proxy @1)
          void (Sync.takeBlock @(Proxy 2))
          Time.sleep (MilliSeconds 100)
          atomicPut 3
    Sync.takeBlock @(Proxy 1)
    async (kill t2)
    Sync.putBlock (Proxy @2)
    _ <- wait t2
    assertEq 2 =<< atomicGet
