module Polysemy.Conc.Async where

import qualified Control.Concurrent.Async as Base
import Polysemy.Async (Async, async, cancel)
import Polysemy.Resource (Resource, bracket)
import Polysemy.Time (MilliSeconds (MilliSeconds), TimeUnit)

import Polysemy.Conc.Effect.Race (Race)
import qualified Polysemy.Conc.Effect.Sync as Sync
import qualified Polysemy.Conc.Race as Race
import Polysemy.Conc.Interpreter.Sync (interpretSync)
import Polysemy.Conc.Effect.Sync (Sync, ScopedSync)
import Polysemy.Conc.Sync (withSync)

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

-- |Run an action with 'async', but don't start it right away, so the thread handle can be processed before the action
-- executes.
--
-- Takes a callback function that is invoked after spawning the thread.
-- The callback receives the 'Base.Async' handle and a unit action that starts the computation.
--
-- This is helpful if the 'Base.Async' has to be stored in state and the same state is written when the action finishes.
-- In that case, the race condition causes the handle to be written over the finished state.
--
-- @
-- makeRequest = put Nothing
--
-- main = scheduleAsync makeRequest \ handle start -> do
--   put (Just handle)
--   start -- now makeRequest is executed
-- @
scheduleAsync ::
  ∀ res b r a .
  Members [ScopedSync res (), Async, Race] r =>
  Sem r b ->
  (Base.Async (Maybe b) -> Sem (Sync () : r) () -> Sem (Sync () : r) a) ->
  Sem r a
scheduleAsync mb f =
  withSync @() @res do
    h <- async do
      Sync.readBlock @()
      raise mb
    f h (Sync.putBlock ())

-- |Variant of 'scheduleAsync' that directly interprets the 'MVar' used for signalling.
scheduleAsyncIO ::
  ∀ b r a .
  Members [Resource, Async, Race, Embed IO] r =>
  Sem r b ->
  (Base.Async (Maybe b) -> Sem (Sync () : r) () -> Sem (Sync () : r) a) ->
  Sem r a
scheduleAsyncIO mb f =
  interpretSync @() do
    h <- async do
      Sync.readBlock @()
      raise mb
    f h (Sync.putBlock ())
