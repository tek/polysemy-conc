-- |Description: Sync Interpreters
module Polysemy.Conc.Interpreter.Sync where

import Control.Concurrent (
  MVar,
  isEmptyMVar,
  newEmptyMVar,
  newMVar,
  putMVar,
  readMVar,
  takeMVar,
  tryPutMVar,
  tryReadMVar,
  tryTakeMVar,
  )

import Polysemy.Conc.Effect.Race (Race)
import Polysemy.Conc.Effect.Scoped (Scoped)
import qualified Polysemy.Conc.Effect.Sync as Sync
import Polysemy.Conc.Effect.Sync (Sync, SyncResources (SyncResources), unSyncResources)
import Polysemy.Conc.Interpreter.Scoped (runScopedAs)
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
      embed (readMVar var)
    Sync.Wait interval ->
      rightToMaybe <$> Race.timeoutAs () interval (embed (readMVar var))
    Sync.Try ->
      embed (tryReadMVar var)
    Sync.TakeBlock ->
      embed (takeMVar var)
    Sync.TakeWait interval ->
      rightToMaybe <$> Race.timeoutAs () interval (embed (takeMVar var))
    Sync.TakeTry ->
      embed (tryTakeMVar var)
    Sync.ReadBlock ->
      embed (readMVar var)
    Sync.ReadWait interval ->
      rightToMaybe <$> Race.timeoutAs () interval (embed (readMVar var))
    Sync.ReadTry ->
      embed (tryReadMVar var)
    Sync.PutBlock d ->
      embed (putMVar var d)
    Sync.PutWait interval d ->
      Race.timeoutAs_ False interval (True <$ embed (putMVar var d))
    Sync.PutTry d ->
      embed (tryPutMVar var d)
    Sync.Empty ->
      embed (isEmptyMVar var)

-- |Interpret 'Sync' with an empty 'MVar'.
interpretSync ::
  ∀ d r .
  Members [Race, Embed IO] r =>
  InterpreterFor (Sync d) r
interpretSync sem = do
  var <- embed newEmptyMVar
  interpretSyncWith var sem

-- |Interpret 'Sync' with an 'MVar' containing the specified value.
interpretSyncAs ::
  ∀ d r .
  Members [Race, Embed IO] r =>
  d ->
  InterpreterFor (Sync d) r
interpretSyncAs d sem = do
  var <- embed (newMVar d)
  interpretSyncWith var sem

-- |Interpret 'Sync' for locally scoped use with an empty 'MVar'.
interpretScopedSync ::
  ∀ d r .
  Members [Resource, Race, Embed IO] r =>
  InterpreterFor (Scoped (SyncResources (MVar d)) (Sync d)) r
interpretScopedSync =
  runScopedAs (SyncResources <$> embed newEmptyMVar) \ r -> interpretSyncWith (unSyncResources r)

-- |Interpret 'Sync' for locally scoped use with an 'MVar' containing the specified value.
interpretScopedSyncAs ::
  ∀ d r .
  Members [Resource, Race, Embed IO] r =>
  d ->
  InterpreterFor (Scoped (SyncResources (MVar d)) (Sync d)) r
interpretScopedSyncAs d =
  runScopedAs (SyncResources <$> embed (newMVar d)) \ r -> interpretSyncWith (unSyncResources r)
