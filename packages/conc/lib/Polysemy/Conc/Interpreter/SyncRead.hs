-- |Description: Interpreter for 'SyncRead' that reinterprets to 'Sync'.
module Polysemy.Conc.Interpreter.SyncRead where

import qualified Polysemy.Conc.Effect.Sync as Sync
import Polysemy.Conc.Effect.Sync (Sync)
import qualified Polysemy.Conc.Effect.SyncRead as SyncRead
import Polysemy.Conc.Effect.SyncRead (SyncRead)

-- |Run 'SyncRead' in terms of 'Sync'.
syncRead ::
  ∀ d r .
  Member (Sync d) r =>
  InterpreterFor (SyncRead d) r
syncRead =
  interpret \case
    SyncRead.Block ->
      Sync.block
    SyncRead.Wait u ->
      Sync.wait u
    SyncRead.Try ->
      Sync.try
    SyncRead.Empty ->
      Sync.empty @d
{-# inline syncRead #-}
