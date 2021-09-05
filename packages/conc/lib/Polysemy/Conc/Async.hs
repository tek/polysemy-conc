module Polysemy.Conc.Async where

import qualified Control.Concurrent.Async as Base
import Polysemy.Async (Async, async, cancel)
import Polysemy.Resource (Resource, bracket)
import Polysemy.Time (MilliSeconds (MilliSeconds), TimeUnit)

import Polysemy.Conc.Effect.Race (Race)
import qualified Polysemy.Conc.Race as Race

-- |Run the first action asynchronously while the second action executes, then cancel the first action.
-- Passes the handle into the action to allow it to await its result.
--
-- When cancelling, this variant will wait indefinitely for the thread to be gone.
withAsyncBlock ::
  Members [Resource, Async] r =>
  Sem r b ->
  (Base.Async (Maybe b) -> Sem r a) ->
  Sem r a
withAsyncBlock mb =
  bracket (async mb) cancel

-- |Run the first action asynchronously while the second action executes, then cancel the first action.
-- Passes the handle into the action to allow it to await its result.
--
-- When cancelling, this variant will wait for the specified interval for the thread to be gone.
withAsyncWait ::
  TimeUnit u =>
  Members [Resource, Race, Async] r =>
  u ->
  Sem r b ->
  (Base.Async (Maybe b) -> Sem r a) ->
  Sem r a
withAsyncWait interval mb =
  bracket (async mb) (Race.timeoutU interval . cancel)

-- |Run the first action asynchronously while the second action executes, then cancel the first action.
-- Passes the handle into the action to allow it to await its result.
--
-- When cancelling, this variant will wait for 500ms for the thread to be gone.
withAsync ::
  Members [Resource, Race, Async] r =>
  Sem r b ->
  (Base.Async (Maybe b) -> Sem r a) ->
  Sem r a
withAsync =
  withAsyncWait (MilliSeconds 500)

-- |Run the first action asynchronously while the second action executes, then cancel the first action.
-- Discards the handle, expecting the async action to either terminate or be cancelled.
--
-- When cancelling, this variant will wait for 500ms for the thread to be gone.
withAsync_ ::
  Members [Resource, Race, Async] r =>
  Sem r b ->
  Sem r a ->
  Sem r a
withAsync_ mb =
  withAsync mb . const
