{-# options_haddock prune #-}

-- |Description: Monitor Interpreters, Internal
module Polysemy.Conc.Interpreter.Monitor where

import qualified Control.Exception as Base
import qualified Polysemy.Time as Time
import Polysemy.Time (Time)

import Polysemy.Conc.Async (withAsync_)
import Polysemy.Conc.Effect.Monitor (
  Monitor (Monitor),
  MonitorCheck (MonitorCheck),
  MonitorResource (MonitorResource),
  RestartingMonitor,
  ScopedMonitor,
  )
import qualified Polysemy.Conc.Effect.Race as Race
import Polysemy.Conc.Effect.Race (Race)
import Polysemy.Conc.Interpreter.Scoped (runScoped, runScopedAs)

newtype CancelResource =
  CancelResource { signal :: MVar () }

data MonitorCancel =
  MonitorCancel
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

interpretMonitorCancel ::
  Members [Race, Async, Final IO] r =>
  MonitorResource CancelResource ->
  InterpreterFor (Monitor action) r
interpretMonitorCancel (MonitorResource CancelResource {..}) =
  interpretH \case
    Monitor ma ->
      either (const (Base.throw MonitorCancel)) pure =<< Race.race (embedFinal @IO (readMVar signal)) (runTSimple ma)

monitorRestart ::
  ∀ t d r a .
  Members [Time t d, Resource, Async, Race, Final IO] r =>
  MonitorCheck r ->
  (MonitorResource CancelResource -> Sem r a) ->
  Sem r a
monitorRestart (MonitorCheck interval check) use = do
  sig <- embedFinal @IO newEmptyMVar
  withAsync_ (Time.loop_ @t @d interval (check sig)) (spin sig)
  where
    spin sig = do
      let res = (MonitorResource (CancelResource sig))
      void (embedFinal @IO (tryTakeMVar sig))
      either (const (spin sig)) pure =<< errorToIOFinal @MonitorCancel (fromExceptionSem @MonitorCancel (raise (use res)))

-- |Interpret @'Polysemy.Conc.Scoped' 'Monitor'@ with the 'Polysemy.Conc.Restart' strategy.
-- This takes a check action that may put an 'MVar' when the scoped region should be restarted.
-- The check is executed in a loop, with an interval given in 'MonitorCheck'.
interpretMonitorRestart ::
  ∀ t d r .
  Members [Time t d, Resource, Async, Race, Final IO] r =>
  MonitorCheck r ->
  InterpreterFor (RestartingMonitor CancelResource) r
interpretMonitorRestart check =
  runScoped (const (monitorRestart @t @d check)) interpretMonitorCancel

interpretMonitorPure' :: MonitorResource () -> InterpreterFor (Monitor action) r
interpretMonitorPure' _ =
  interpretH \case
    Monitor ma ->
      runTSimple ma

-- |Run 'Monitor' as a no-op.
interpretMonitorPure :: InterpreterFor (ScopedMonitor () action) r
interpretMonitorPure =
  runScopedAs (const (pure (MonitorResource ()))) interpretMonitorPure'
