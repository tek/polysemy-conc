{-# options_haddock prune #-}
{-# language FieldSelectors #-}

-- | Description: Interrupt interpreters
module Polysemy.Process.Interpreter.Interrupt where

import qualified Control.Concurrent.Async as A
import Control.Concurrent.Async (AsyncCancelled)
import Control.Concurrent.STM (TVar, newTVarIO)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text.IO as Text
import qualified Polysemy.Conc.Effect.Critical as Critical
import Polysemy.Conc.Effect.Critical (Critical)
import Polysemy.Conc.Effect.Race (Race)
import qualified Polysemy.Conc.Effect.Sync as Sync
import Polysemy.Conc.Interpreter.Sync (interpretSync)
import Polysemy.Conc.Race (race_)
import Polysemy.Internal.Tactics (liftT)
import Polysemy.Time (Seconds (Seconds))
import System.IO (stderr)
import System.Posix.Signals (
  Handler (Catch, CatchInfo, CatchInfoOnce, CatchOnce),
  SignalInfo,
  installHandler,
  keyboardSignal,
  )

import Polysemy.Process.Effect.Interrupt (Interrupt (..))

putErr ::
  Member (Embed IO) r =>
  Bool ->
  Text ->
  Sem r ()
putErr = \case
  True -> embed . Text.hPutStrLn stderr
  False -> const unit

data InterruptState =
  InterruptState {
    quit :: !(MVar ()),
    finished :: !(MVar ()),
    listeners :: !(Set Text),
    original :: !(SignalInfo -> IO ()),
    handlers :: !(Map Text (IO ()))
  }

modListeners :: (Set Text -> Set Text) -> InterruptState -> InterruptState
modListeners f s@InterruptState {listeners} =
  s {listeners = f listeners}

modHandlers :: (Map Text (IO ()) -> Map Text (IO ())) -> InterruptState -> InterruptState
modHandlers f s@InterruptState {handlers} =
  s {handlers = f handlers}

waitQuit ::
  Members [AtomicState InterruptState, Embed IO] r =>
  Sem r ()
waitQuit = do
  mv <- atomicGets quit
  embed (readMVar mv)

checkListeners ::
  Members [AtomicState InterruptState, Embed IO] r =>
  Sem r ()
checkListeners =
  whenM (atomicGets (Set.null . listeners)) do
    fin <- atomicGets finished
    void (embed (tryPutMVar fin ()))

onQuit ::
  Members [AtomicState InterruptState, Embed IO] r =>
  Text ->
  Sem r a ->
  Sem r a
onQuit name ma = do
  atomicModify' (modListeners (Set.insert name))
  waitQuit
  a <- ma
  atomicModify' (modListeners (Set.delete name))
  checkListeners
  pure a

processHandler ::
  Member (Embed IO) r =>
  Bool ->
  Text ->
  IO () ->
  Sem r ()
processHandler verbose name thunk = do
  putErr verbose ("processing interrupt handler: " <> name)
  embed thunk

execInterrupt ::
  Members [AtomicState InterruptState, Embed IO] r =>
  Bool ->
  Sem r (SignalInfo -> Sem r ())
execInterrupt verbose = do
  InterruptState quitSignal finishSignal _ orig _ <- atomicGet
  whenM (embed (tryPutMVar quitSignal ())) do
    traverse_ (uncurry (processHandler verbose)) . Map.toList =<< atomicGets handlers
    checkListeners
    embed (takeMVar finishSignal)
  embed . orig <$ putErr verbose "interrupt handlers finished"

registerHandler ::
  Member (AtomicState InterruptState) r =>
  Text ->
  IO () ->
  Sem r ()
registerHandler name handler =
  atomicModify' (modHandlers (Map.insert name handler))

awaitOrKill ::
  Members [AtomicState InterruptState, Critical, Race, Async, Embed IO] r =>
  Bool ->
  Text ->
  A.Async (Maybe a) ->
  Sem r (Maybe a)
awaitOrKill verbose desc handle = do
  interpretSync @() do
    race_ (catchCritical (await handle)) kill
  where
    catchCritical =
      maybe waitKill (pure . Just) <=< Critical.catchAs @AsyncCancelled Nothing
    waitKill =
      Nothing <$ Sync.wait @() (Seconds 1)
    kill = do
      onQuit desc do
        putErr verbose ("killing " <> desc)
        cancel handle
        putErr verbose ("killed " <> desc)
        Sync.putBlock ()
        pure Nothing

interpretInterruptState ::
  Members [AtomicState InterruptState, Critical, Race, Async, Embed IO] r =>
  Bool ->
  InterpreterFor Interrupt r
interpretInterruptState verbose =
  interpretH \case
    Register name handler ->
      liftT (registerHandler name handler)
    Unregister name ->
      liftT $ atomicModify' \ s@InterruptState {handlers} -> s {handlers = Map.delete name handlers}
    WaitQuit ->
      liftT waitQuit
    Quit ->
      liftT do
        putErr verbose "manual interrupt"
        void (execInterrupt verbose)
    Interrupted ->
      liftT . fmap isJust . embed . tryReadMVar =<< atomicGets quit
    KillOnQuit desc ma -> do
      maT <- runT ma
      ins <- getInspectorT
      handle <- raise (interpretInterruptState verbose (async maT))
      result <- liftT (awaitOrKill verbose desc handle)
      pure (join . fmap (inspect ins) <$> result)
{-# inline interpretInterruptState #-}

broadcastInterrupt ::
  Members [AtomicState InterruptState, Embed IO] r =>
  Bool ->
  SignalInfo ->
  Sem r ()
broadcastInterrupt verbose sig = do
  putErr verbose "caught interrupt signal"
  orig <- execInterrupt verbose
  orig sig

-- The original handler is either the default handler that kills all threads or a handler installed by an environment
-- like ghcid.
-- In the latter case, not calling it results in ghcid misbehaving.
-- To distinguish the two cases, the constructor used by the default is 'Catch', while a custom handler should usually
-- use 'CatchOnce', since you don't want to catch repeated occurences of SIGINT, as it will surely cause problems.
originalHandler :: Handler -> (SignalInfo -> IO ())
originalHandler (CatchOnce thunk) =
  (const thunk)
originalHandler (CatchInfoOnce thunk) =
  thunk
originalHandler (Catch thunk) =
  (const thunk)
originalHandler (CatchInfo thunk) =
  thunk
originalHandler _ =
  const unit
{-# inline originalHandler #-}

installSignalHandler ::
  Bool ->
  TVar InterruptState ->
  ((SignalInfo -> IO ()) -> Handler) ->
  IO Handler
installSignalHandler verbose state consHandler =
  installHandler keyboardSignal (consHandler handler) Nothing
  where
    handler sig =
      runFinal $ embedToFinal @IO $ runAtomicStateTVar state (broadcastInterrupt verbose sig)

-- | Interpret 'Interrupt' by installing a signal handler.
--
-- Takes a constructor for 'Handler'.
interpretInterruptWith' ::
  Members [Critical, Race, Async, Embed IO] r =>
  Bool ->
  ((SignalInfo -> IO ()) -> Handler) ->
  InterpreterFor Interrupt r
interpretInterruptWith' verbose consHandler sem = do
  quitMVar <- embed newEmptyMVar
  finishMVar <- embed newEmptyMVar
  state <- embed (newTVarIO (InterruptState quitMVar finishMVar Set.empty (const unit) Map.empty))
  orig <- embed $ installSignalHandler verbose state consHandler
  runAtomicStateTVar state do
    atomicModify' \ s -> s {original = originalHandler orig}
    interpretInterruptState verbose $ raiseUnder sem

interpretInterruptWith ::
  Members [Critical, Race, Async, Embed IO] r =>
  ((SignalInfo -> IO ()) -> Handler) ->
  InterpreterFor Interrupt r
interpretInterruptWith = interpretInterruptWith' True

-- | Interpret 'Interrupt' by installing a signal handler.
--
-- Catches repeat invocations of SIGINT.
interpretInterrupt' ::
  Members [Critical, Race, Async, Embed IO] r =>
  Bool ->
  InterpreterFor Interrupt r
interpretInterrupt' verbose =
  interpretInterruptWith' verbose CatchInfo

-- | Interpret 'Interrupt' by installing a signal handler.
--
-- Catches repeat invocations of SIGINT.
interpretInterrupt ::
  Members [Critical, Race, Async, Embed IO] r =>
  InterpreterFor Interrupt r
interpretInterrupt =
  interpretInterrupt' True

-- | Interpret 'Interrupt' by installing a signal handler.
--
-- Catches only the first invocation of SIGINT.
interpretInterruptOnce' ::
  Members [Critical, Race, Async, Embed IO] r =>
  Bool ->
  InterpreterFor Interrupt r
interpretInterruptOnce' verbose =
  interpretInterruptWith' verbose CatchInfoOnce

-- | Interpret 'Interrupt' by installing a signal handler.
--
-- Catches only the first invocation of SIGINT.
interpretInterruptOnce ::
  Members [Critical, Race, Async, Embed IO] r =>
  InterpreterFor Interrupt r
interpretInterruptOnce =
  interpretInterruptOnce' True

-- | Eliminate 'Interrupt' without interpreting.
interpretInterruptNull ::
  InterpreterFor Interrupt r
interpretInterruptNull =
  interpretH \case
    Register _ _ ->
      pureT ()
    Unregister _ ->
      pureT ()
    WaitQuit ->
      pureT ()
    Quit ->
      pureT ()
    Interrupted ->
      pureT False
    KillOnQuit _ _ ->
      pureT Nothing
