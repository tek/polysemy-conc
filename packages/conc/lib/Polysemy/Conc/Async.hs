module Polysemy.Conc.Async where

import Polysemy.Async (Async, async, cancel)
import Polysemy.Resource (Resource, bracket)
import qualified Control.Concurrent.Async as Base

-- |Run the first action asynchronously while the second action executes, then cancel the first action.
-- Passes the handle into the action to allow it to await its result.
withAsync ::
  Members [Resource, Async] r =>
  Sem r b ->
  (Base.Async (Maybe b) -> Sem r a) ->
  Sem r a
withAsync mb =
  bracket (async mb) cancel

-- |Run the first action asynchronously while the second action executes, then cancel the first action.
-- Discards the handle, expecting the async action to either terminate or be cancelled.
withAsync_ ::
  Members [Resource, Async] r =>
  Sem r b ->
  Sem r a ->
  Sem r a
withAsync_ mb =
  withAsync mb . const
