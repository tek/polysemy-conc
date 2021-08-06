module Polysemy.Conc.Async where

import Polysemy.Async (Async, async, cancel)
import Polysemy.Resource (Resource, bracket)

-- |Run the first action asynchronously while the second action executes, then cancel the first action.
withAsync ::
  Members [Resource, Async] r =>
  Sem r b ->
  Sem r a ->
  Sem r a
withAsync ma =
  bracket (async ma) cancel . const
