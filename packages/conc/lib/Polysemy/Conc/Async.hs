module Polysemy.Conc.Async where

import Polysemy.Async (Async, async, cancel)
import Polysemy.Resource (Resource, bracket)

withAsync ::
  Members [Resource, Async] r =>
  Sem r b ->
  Sem r a ->
  Sem r a
withAsync ma =
  bracket (async ma) cancel . const
