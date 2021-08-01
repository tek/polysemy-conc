-- |Description: Sync interpreters
module Polysemy.Conc.Interpreter.Sync where

import Control.Concurrent (isEmptyMVar)

import qualified Polysemy.Conc.Data.Race as Race
import Polysemy.Conc.Data.Race (Race)
import qualified Polysemy.Conc.Effect.Sync as Sync
import Polysemy.Conc.Effect.Sync (Sync)
import qualified Polysemy.Conc.Race as Race

-- |Interpret 'Sync' with the provided 'MVar'.
interpretSyncWith ::
  ∀ d r .
  Members [Race, Embed IO] r =>
  MVar d ->
  InterpreterFor (Sync d) r
interpretSyncWith var =
  interpret \case
    Sync.Block ->
      readMVar var
    Sync.Wait interval ->
      rightToMaybe <$> Race.timeout () interval (readMVar var)
    Sync.Try ->
      tryReadMVar var
    Sync.TakeBlock ->
      takeMVar var
    Sync.TakeWait interval ->
      rightToMaybe <$> Race.timeout () interval (takeMVar var)
    Sync.TakeTry ->
      tryTakeMVar var
    Sync.PutBlock d ->
      putMVar var d
    Sync.PutWait interval d ->
      Race.timeout_ False interval (True <$ putMVar var d)
    Sync.PutTry d ->
      tryPutMVar var d
    Sync.Empty ->
      embed (isEmptyMVar var)

-- |Interpret 'Sync' with an empty 'MVar'.
interpretSync ::
  ∀ d r .
  Members [Race, Embed IO] r =>
  InterpreterFor (Sync d) r
interpretSync sem = do
  var <- newEmptyMVar
  interpretSyncWith var sem

-- |Interpret 'Sync' with an 'MVar' containing the specified value.
interpretSyncAs ::
  ∀ d r .
  Members [Race, Embed IO] r =>
  d ->
  InterpreterFor (Sync d) r
interpretSyncAs d sem = do
  var <- newMVar d
  interpretSyncWith var sem
