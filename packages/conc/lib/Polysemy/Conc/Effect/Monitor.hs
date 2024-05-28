{-# options_haddock prune #-}

-- | Description: Monitor Effect, Internal
module Polysemy.Conc.Effect.Monitor where

import Polysemy.Time (NanoSeconds)

-- | Marker type for the restarting action for 'Monitor'.
data Restart =
  Restart
  deriving stock (Eq, Show)

-- | Mark a region as being subject to intervention by a monitoring program.
-- This can mean that a thread is repeatedly checking a condition and cancelling this region when it is unmet.
-- A use case could be checking whether a remote service is available, or whether the system was suspended and resumed.
-- This should be used in a 'Scoped_' context, like 'withMonitor'.
data Monitor (action :: Type) :: Effect where
  Monitor :: m a -> Monitor action m a

makeSem_ ''Monitor

-- | Mark a region as being subject to intervention by a monitoring program.
monitor ::
  ∀ action r a .
  Member (Monitor action) r =>
  Sem r a ->
  Sem r a

-- | Convenience alias for a 'Scoped_' 'Monitor'.
type ScopedMonitor (action :: Type) =
  Scoped_ (Monitor action)

-- | 'Monitor' specialized to the 'Restart' action.
type RestartingMonitor =
  ScopedMonitor Restart

-- | Resources for a 'Scoped_' 'Monitor'.
data MonitorCheck r =
  MonitorCheck {
    interval :: NanoSeconds,
    check :: Sem r Bool
  }

-- | Transform the stack of the check in a 'MonitorCheck'.
hoistMonitorCheck ::
  (∀ x . Sem r x -> Sem r' x) ->
  MonitorCheck r ->
  MonitorCheck r'
hoistMonitorCheck f MonitorCheck {..} =
  MonitorCheck {check = f check, ..}

-- | Start a region that can contain monitor-intervention regions.
withMonitor ::
  ∀ action r .
  Member (ScopedMonitor action) r =>
  InterpreterFor (Monitor action) r
withMonitor =
  scoped_

-- | Variant of 'withMonitor' that uses the 'Restart' strategy.
restart ::
  Member (ScopedMonitor Restart) r =>
  InterpreterFor (Monitor Restart) r
restart =
  withMonitor
