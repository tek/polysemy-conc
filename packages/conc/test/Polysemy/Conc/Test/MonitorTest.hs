{-# options_ghc -fplugin=Polysemy.Plugin #-}

module Polysemy.Conc.Test.MonitorTest where

import Control.Concurrent (threadDelay)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Polysemy.Test (TestError (TestError), UnitTest, assertEq, assertJust, runTestAuto)
import qualified Polysemy.Time as Time
import Polysemy.Time (
  GhcTime,
  Hours (Hours),
  MilliSeconds (MilliSeconds),
  Minutes (Minutes),
  NanoSeconds (NanoSeconds),
  Seconds (Seconds),
  Time,
  TimeUnit,
  convert,
  interpretTimeGhc,
  interpretTimeGhcConstantNow,
  )
import Torsor (difference)

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
  Sem r Bool
checker =
  True <$ Sync.takeBlock

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

locked ::
  Members [AtomicState [Text], Error TestError, Embed IO] r =>
  Sem r a
locked = do
  embed (threadDelay 1_000_000)
  msgs <- atomicGet
  throw (TestError (Text.unlines ("Locked after:" : reverse msgs)))

takeBlock ::
  ∀ label r .
  HasCallStack =>
  Members [AtomicState [Text], Sync (Proxy label), Error TestError, Embed IO] r =>
  Text ->
  Sem r ()
takeBlock desc =
  withFrozenCallStack do
    atomicModify' (("taking '" <> desc <> "'") :)
    whenM (isNothing <$> Sync.takeWait (Seconds 1)) locked

putBlock ::
  ∀ label r .
  HasCallStack =>
  Members [AtomicState [Text], Sync (Proxy label), Error TestError, Embed IO] r =>
  Text ->
  Sem r ()
putBlock desc =
  withFrozenCallStack do
    atomicModify' (("putting '" <> desc <> "'") :)
    unlessM (Sync.putWait (Seconds 1) Proxy) locked

progSkew ::
  Members [Sync (Proxy "start"), Sync (Proxy "1"), Sync (Proxy "2"), Sync (Proxy "3"), Embed IO] r =>
  Members [Monitor Restart, AtomicState [Text], AtomicState Int, Error TestError, Time t d] r =>
  Sem r Int
progSkew =
  Monitor.monitor do
    atomicModify' (1 +)
    putBlock @"start" "start"
    takeBlock @"1" "1"
    putBlock @"2" "2"
    takeBlock @"3" "3"
    atomicGet

sleepPoll ::
  Members [GhcTime, Embed IO] r =>
  TimeUnit u =>
  u ->
  UTCTime ->
  Sem r ()
sleepPoll duration start =
  spin
  where
    spin = do
      embed (threadDelay 10_000)
      unlessM (later <$> Time.now) spin
    later now =
      difference now start >= diff
    diff =
      convert duration

interceptTime ::
  ∀ r a .
  Members [AtomicState [Text], GhcTime, Sync (Proxy "sleep"), Error TestError, Embed IO] r =>
  Sem r a ->
  Sem r a
interceptTime =
  intercept \case
    Time.Now -> do
      atomicModify' ("now" :)
      Time.now
    Time.Today ->
      Time.today
    Time.Sleep t -> do
      atomicModify' ("sleep" :)
      now <- Time.now
      putBlock @"sleep" "sleep"
      sleepPoll t now
      atomicModify' ("slept" :)
    Time.SetTime now ->
      Time.setTime now
    Time.Adjust (diff :: u) -> do
      atomicModify' ("adjust" :)
      send (Time.Adjust diff)
    Time.SetDate startAt ->
      Time.setDate startAt

test_monitorClockSkew :: UnitTest
test_monitorClockSkew =
  runTestAuto $
  asyncToIOFinal $
  interpretRace $
  interpretAtomic (0 :: Int) $
  interpretAtomic ([] :: [Text]) $
  interpretAtomic @(Maybe UTCTime) Nothing $
  interpretSync @(Proxy "start") $
  interpretSync @(Proxy "sleep") $
  interpretSync @(Proxy "1") $
  interpretSync @(Proxy "2") $
  interpretSync @(Proxy "3") $
  interpretTimeGhcConstantNow $
  interceptTime $
  interpretMonitorRestart monitor do
    h <- async (Monitor.restart progSkew)
    takeBlock @"start" "start 1"
    takeBlock @"sleep" "sleep"
    Time.adjust (Hours 1)
    _ <- takeBlock @"start" "start 2"
    takeBlock @"sleep" "sleep"
    Time.adjust (Minutes 10)
    putBlock @"1" "1 1"
    takeBlock @"2" "2 1"
    takeBlock @"sleep" "sleep"
    Time.adjust (Hours 1)
    _ <- takeBlock @"start" "start 3"
    putBlock @"1" "1 2"
    takeBlock @"2" "2 2"
    putBlock @"3" "3"
    assertJust 3 =<< await h
  where
    monitor = monitorClockSkew (clockSkewConfig (MilliSeconds 1) (Minutes 30))
