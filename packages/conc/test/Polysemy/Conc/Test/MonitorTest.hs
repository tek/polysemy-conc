{-# options_ghc -fplugin=Polysemy.Plugin #-}

module Polysemy.Conc.Test.MonitorTest where

import Data.Time (UTCTime)
import Polysemy.Test (UnitTest, assertEq, assertJust, runTestAuto)
import qualified Polysemy.Time as Time
import Polysemy.Time (
  Hours (Hours),
  MilliSeconds (MilliSeconds),
  Minutes (Minutes),
  NanoSeconds (NanoSeconds),
  Time,
  interpretTimeGhc,
  interpretTimeGhcConstantNow,
  )

import Polysemy.Conc.AtomicState (interpretAtomic)
import qualified Polysemy.Conc.Effect.Monitor as Monitor
import Polysemy.Conc.Effect.Monitor (Monitor, MonitorCheck (MonitorCheck), Restart)
import qualified Polysemy.Conc.Effect.Sync as Sync
import Polysemy.Conc.Effect.Sync (Sync)
import Polysemy.Conc.Interpreter.Monitor (interpretMonitorRestart)
import Polysemy.Conc.Interpreter.Race (interpretRace)
import Polysemy.Conc.Interpreter.Sync (interpretSync)
import Polysemy.Conc.Monitor (clockSkewConfig, monitorClockSkew)

prog ::
  Members [Monitor Restart, Time t d, AtomicState Int, Sync ()] r =>
  Sem r Int
prog = do
  Monitor.monitor do
    atomicModify' (1 +)
    i <- atomicGet
    when (i <= 2) do
      Sync.putBlock ()
      Time.sleep (Hours 1)
    pure i

checker ::
  Members [Time t d, Sync (), Embed IO] r =>
  MVar () ->
  Sem r ()
checker signal =
  Sync.takeBlock *> embed (putMVar signal ())

test_monitorBasic :: UnitTest
test_monitorBasic =
  runTestAuto $
  asyncToIOFinal $
  interpretRace $
  interpretTimeGhc $
  interpretAtomic 0 $
  interpretSync $
  interpretMonitorRestart (MonitorCheck (NanoSeconds 0) checker) do
    assertEq 3 =<< Monitor.restart prog

progSkew ::
  Member (Sync (Proxy "start")) r =>
  Members [Monitor Restart, Time t d, AtomicState Int, Sync (Proxy 1), Sync (Proxy 2), Sync (Proxy 3)] r =>
  Sem r Int
progSkew = do
  Monitor.monitor do
    atomicModify' (1 +)
    Sync.putBlock (Proxy @"start")
    void $ Sync.takeBlock @(Proxy 1)
    Sync.putBlock (Proxy @2)
    void $ Sync.takeBlock @(Proxy 3)
    atomicGet

test_monitorClockSkew :: UnitTest
test_monitorClockSkew =
  runTestAuto $
  asyncToIOFinal $
  interpretRace $
  interpretTimeGhcConstantNow $
  interpretAtomic (0 :: Int) $
  interpretAtomic @(Maybe UTCTime) Nothing $
  interpretSync @(Proxy "start") $
  interpretSync @(Proxy 1) $
  interpretSync @(Proxy 2) $
  interpretSync @(Proxy 3) $
  interpretMonitorRestart (monitorClockSkew (clockSkewConfig (MilliSeconds 1) (Minutes 30))) do
    h <- async (Monitor.restart progSkew)
    _ <- Sync.takeBlock @(Proxy "start")
    Time.adjust (Hours 1)
    _ <- Sync.takeBlock @(Proxy "start")
    Time.adjust (Minutes 10)
    Sync.putBlock (Proxy @1)
    Sync.takeBlock @(Proxy 2)
    Time.adjust (Hours 1)
    _ <- Sync.takeBlock @(Proxy "start")
    Sync.putBlock (Proxy @1)
    Sync.takeBlock @(Proxy 2)
    Sync.putBlock (Proxy @3)
    assertJust 3 =<< await h
