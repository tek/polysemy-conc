{-# options_haddock prune #-}
-- |Description: Monitor Effect, Internal
module Polysemy.Conc.Effect.Monitor where

import Polysemy (makeSem_)
import Polysemy.Time (NanoSeconds)

import Polysemy.Conc.Effect.Scoped (Scoped, scoped)

-- |Marker type for the restarting action for 'Monitor'.
data Restart =
  Restart
  deriving stock (Eq, Show)

-- |Mark a region as being subject to intervention by a monitoring program.
-- This can mean that a thread is repeatedly checking a condition and cancelling this region when it is unmet.
-- A use case could be checking whether a remote service is available, or whether the system was suspended and resumed.
-- This should be used in a 'Scoped' context, like 'withMonitor'.
data Monitor (action :: Type) :: Effect where
  Monitor :: m a -> Monitor action m a

makeSem_ ''Monitor

-- |Mark a region as being subject to intervention by a monitoring program.
monitor ::
  ∀ action r a .
  Member (Monitor action) r =>
  Sem r a ->
  Sem r a

-- |Marker type for a 'Scoped' 'Monitor'.
newtype MonitorResource a =
  MonitorResource { unMonitorResource :: a }

-- |Convenience alias for a 'Scoped' 'Monitor'.
type ScopedMonitor (resource :: Type) (action :: Type) =
  Scoped (MonitorResource resource) (Monitor action)

-- |'Monitor' specialized to the 'Restart' action.
type RestartingMonitor (resource :: Type) =
  ScopedMonitor resource Restart

-- |Resources for a 'Scoped' 'Monitor'.
data MonitorCheck r =
  MonitorCheck {
    interval :: NanoSeconds,
    check :: MVar () -> Sem r ()
  }

-- |Start a region that can contain monitor-intervention regions.
withMonitor ::
  ∀ resource action r .
  Member (ScopedMonitor resource action) r =>
  InterpreterFor (Monitor action) r
withMonitor =
  scoped @(MonitorResource resource)

-- |Variant of 'withMonitor' that uses the 'Restart' strategy.
restart ::
  ∀ resource r .
  Member (ScopedMonitor resource Restart) r =>
  InterpreterFor (Monitor Restart) r
restart =
  withMonitor @resource
