{-# options_ghc -fplugin=Polysemy.Plugin #-}

module Polysemy.Conc.Test.LockTest where

import Polysemy.Test (UnitTest, assert, assertJust, runTestAuto, unitTest)
import Polysemy.Time (GhcTime, MilliSeconds (MilliSeconds), Seconds (Seconds), interpretTimeGhc)
import Test.Tasty (TestTree, testGroup)

import qualified Polysemy.Conc.Effect.Lock as Lock
import Polysemy.Conc.Effect.Lock (Lock)
import Polysemy.Conc.Effect.Mask (Mask)
import Polysemy.Conc.Effect.Race (Race)
import qualified Polysemy.Conc.Effect.Sync as Sync
import Polysemy.Conc.Interpreter.Lock (interpretLockReentrant)
import Polysemy.Conc.Interpreter.Mask (interpretMaskFinal)
import Polysemy.Conc.Interpreter.Race (interpretRace)
import Polysemy.Conc.Interpreter.Sync (interpretSync)
import Polysemy.Conc.Race (timeout_)

interpretLockTest ::
  Members [Resource, Embed IO, Final IO] r =>
  InterpretersFor [Lock, Mask, Race, Async, GhcTime] r
interpretLockTest =
  interpretTimeGhc .
  asyncToIOFinal .
  interpretRace .
  interpretMaskFinal .
  interpretLockReentrant

test_lockBasic :: UnitTest
test_lockBasic =
  runTestAuto $ interpretLockTest $ interpretSync @(Proxy 1) $ interpretSync @(Proxy 2) do
    t1 <- async do
      Lock.lock do
        assert =<< Sync.putTry (Proxy @1)
        assertJust Proxy =<< Sync.wait @(Proxy 2) (Seconds 5)
    assertJust Proxy =<< Sync.wait @(Proxy 1) (Seconds 5)
    assert =<< Lock.lockOr (pure True) (pure False)
    assert =<< timeout_ (pure True) (MilliSeconds 50) (Lock.lock (pure False))
    assert =<< Sync.putTry (Proxy @2)
    assertJust () =<< await t1

test_lockReentry :: UnitTest
test_lockReentry =
  runTestAuto $ interpretLockTest do
    Lock.lock do
      assert =<< Lock.lockOr (pure False) (pure True)
      assertJust True =<< await =<< async do
        assert =<< timeout_ (pure True) (MilliSeconds 50) (Lock.lock (pure False))
        Lock.lockOr (pure True) (pure False)

test_lock :: TestTree
test_lock =
  testGroup "lock" [
    unitTest "basic" test_lockBasic,
    unitTest "reentry" test_lockReentry
  ]
