{-# options_haddock prune #-}

-- |Description: Process Interpreters, Internal
module Polysemy.Process.Interpreter.Process where

import Control.Concurrent.STM.TBMQueue (TBMQueue)
import Data.ByteString (hGetSome, hPut)
import Polysemy.Conc.Async (withAsync_)
import qualified Polysemy.Conc.Data.QueueResult as QueueResult
import qualified Polysemy.Conc.Effect.Queue as Queue
import Polysemy.Conc.Effect.Queue (Queue)
import Polysemy.Conc.Effect.Race (Race)
import qualified Polysemy.Conc.Effect.Sync as Sync
import Polysemy.Conc.Effect.Sync (Sync)
import Polysemy.Conc.Interpreter.Queue.TBM (interpretQueueTBMWith, withTBMQueue)
import Polysemy.Conc.Interpreter.Sync (interpretSync)
import qualified Polysemy.Conc.Race as Conc (timeout_)
import Polysemy.Input (Input (Input))
import Polysemy.Opaque (Opaque)
import Polysemy.Output (Output (Output))
import Polysemy.Resume (
  Stop,
  interpretResumable,
  interpretScopedResumableWith_,
  mapStop,
  restop,
  resumeHoist,
  resumeOr,
  resume_,
  stop,
  stopNote,
  type (!!),
  )
import Polysemy.Scoped (Scoped, Scoped_)
import Prelude hiding (fromException)
import System.IO (BufferMode (NoBuffering), Handle, hSetBuffering, stdin, stdout)

import qualified Polysemy.Process.Data.ProcessError as ProcessError
import Polysemy.Process.Data.ProcessError (ProcessError (Exit, Unknown))
import Polysemy.Process.Data.ProcessKill (ProcessKill (KillAfter, KillImmediately, KillNever))
import Polysemy.Process.Data.ProcessOptions (ProcessOptions (ProcessOptions))
import qualified Polysemy.Process.Data.SystemProcessError as SystemProcessError
import Polysemy.Process.Data.SystemProcessError (SystemProcessError, SystemProcessScopeError)
import qualified Polysemy.Process.Effect.Process as Process
import Polysemy.Process.Effect.Process (Process)
import qualified Polysemy.Process.Effect.ProcessInput as ProcessInput
import Polysemy.Process.Effect.ProcessInput (ProcessInput)
import qualified Polysemy.Process.Effect.ProcessOutput as ProcessOutput
import Polysemy.Process.Effect.ProcessOutput (OutputPipe (Stderr, Stdout), ProcessOutput)
import qualified Polysemy.Process.Effect.SystemProcess as SystemProcess
import Polysemy.Process.Effect.SystemProcess (SystemProcess, withSystemProcess, withSystemProcess_)
import Polysemy.Process.Interpreter.ProcessIO (ProcessIO)
import Polysemy.Process.Interpreter.SystemProcess (
  SysProcConf,
  interpretSystemProcessNative,
  interpretSystemProcessNative_,
  )

newtype In a =
  In { unIn :: a }
  deriving stock (Eq, Show)

newtype Out a =
  Out { unOut :: a }
  deriving stock (Eq, Show)

newtype Err a =
  Err { unErr :: a }
  deriving stock (Eq, Show)

data ProcessQueues i o =
  ProcessQueues {
    pqIn :: TBMQueue (In i),
    pqOut :: TBMQueue (Out o)
  }

terminated ::
  Members [SystemProcess !! SystemProcessError, Stop ProcessError] r =>
  Text ->
  Sem r a
terminated site =
  stop . Exit =<< resumeHoist @_ @SystemProcess fatal SystemProcess.wait
  where
    fatal = \case
      SystemProcessError.Terminated reason ->
        Unknown (site <> ": wait failed (" <> reason <> ")")
      SystemProcessError.NoPipes ->
        Unknown (site <> ": no pipes")

interpretQueues ::
  Members [Resource, Race, Embed IO] r =>
  ProcessQueues i o ->
  InterpretersFor [Queue (In i), Queue (Out o)] r
interpretQueues (ProcessQueues inQ outQ) =
  interpretQueueTBMWith outQ .
  interpretQueueTBMWith inQ

handleProcessWithQueues ::
  ∀ i o m r a .
  Members [Queue (In i), Queue (Out o), Stop ProcessError] r =>
  (∀ x . Text -> Sem r x) ->
  Process i o m a ->
  Sem r a
handleProcessWithQueues onError = \case
  Process.Recv ->
    Queue.read >>= \case
      QueueResult.Closed ->
        onError "recv: closed"
      QueueResult.NotAvailable ->
        onError "recv: impossible: empty"
      QueueResult.Success (Out msg) ->
        pure msg
  Process.Send msg -> do
    whenM (Queue.closed @(In i)) (onError "send: closed")
    Queue.write (In msg)

withSTMResources ::
  ∀ i o r a .
  Members [Resource, Embed IO] r =>
  Int ->
  (ProcessQueues i o -> Sem r a) ->
  Sem r a
withSTMResources qSize action = do
  withTBMQueue qSize \ inQ ->
    withTBMQueue qSize \ outQ ->
      action (ProcessQueues inQ outQ)

withQueues ::
  ∀ i o r .
  Members [Race, Resource, Embed IO] r =>
  Int ->
  InterpretersFor [Queue (In i), Queue (Out o)] r
withQueues qSize action =
  withSTMResources qSize \ qs -> interpretQueues qs action

-- |Call a chunk reading action repeatedly, pass the bytes to 'ProcessOutput' and enqueue its results.
-- As soon as an empty chunk is encountered, the queue is closed if the gating action returns 'False'.
-- The conditional closing is for the purpose of keeping the queue alive until the last producer has written all
-- received chunks – this is important when both stdout and stderr are written to the same queue.
-- When a process writes to stdout and terminates, the stderr reader will immediately receive an empty chunk and close
-- the queue while the stdout reader calls 'ProcessOutput', after which 'Queue.write' will fail and the consumer won't
-- receive the output.
outputQueue ::
  ∀ p chunk err eff r .
  Members [eff !! err, ProcessOutput p chunk, Queue (Out chunk), Embed IO] r =>
  Bool ->
  Sem r Bool ->
  Sem (eff : r) ByteString ->
  Sem r ()
outputQueue discardWhenFull dontClose readChunk = do
  spin ""
  where
    spin buffer =
      resumeOr @err readChunk (write buffer) (const close)
    write buffer msg = do
      (chunks, newBuffer) <- ProcessOutput.chunk @p @chunk buffer msg
      for_ chunks \ (Out -> c) ->
        if discardWhenFull then void (Queue.tryWrite c) else Queue.write c
      spin newBuffer
    close =
      unlessM dontClose (Queue.close @(Out chunk))

inputQueue ::
  ∀ i err eff r .
  Members [eff !! err, ProcessInput i, Queue (In i), Embed IO] r =>
  (ByteString -> Sem (eff : r) ()) ->
  Sem r ()
inputQueue writeChunk =
  spin
  where
    spin =
      Queue.read @(In i) >>= \case
        QueueResult.Success (In msg) -> do
          bytes <- ProcessInput.encode msg
          resumeOr @err (writeChunk bytes) (const spin) (const (Queue.close @(In i)))
        _ ->
          unit

handleKill ::
  Members [SystemProcess, Race] r =>
  ProcessKill ->
  Sem r ()
handleKill = \case
  KillAfter interval ->
    Conc.timeout_ SystemProcess.term interval (void SystemProcess.wait)
  KillImmediately ->
    SystemProcess.term
  KillNever ->
    void SystemProcess.wait

withKill ::
  ∀ err r a .
  Members [SystemProcess !! err, Resource, Race] r =>
  ProcessKill ->
  Sem r a ->
  Sem r a
withKill kill ma =
  finally ma $ resume_ @err @SystemProcess do
    void SystemProcess.pid
    handleKill kill

type ScopeEffects i o err =
  [Queue (In i), Queue (Out o), Sync (), SystemProcess !! err]

queues ::
  ∀ err i o r .
  Member (SystemProcess !! err) r =>
  Members [ProcessInput i, ProcessOutput 'Stdout o, ProcessOutput 'Stderr o, Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  InterpretersFor [Queue (In i), Queue (Out o), Sync ()] r
queues (ProcessOptions discard qSize kill) =
  interpretSync .
  withQueues qSize .
  withAsync_ (outputQueue @'Stderr @o @err @SystemProcess discard (Sync.putTry ()) SystemProcess.readStderr) .
  withAsync_ (outputQueue @'Stdout @o @err @SystemProcess discard (Sync.putTry ()) SystemProcess.readStdout) .
  withAsync_ (inputQueue @i @err @SystemProcess SystemProcess.writeStdin) .
  withKill @err kill

scope ::
  ∀ serr err i o r .
  Members [Scoped_ (SystemProcess !! err) !! serr, Stop serr] r =>
  Members [ProcessInput i, ProcessOutput 'Stdout o, ProcessOutput 'Stderr o, Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  InterpretersFor (ScopeEffects i o err) r
scope options =
  restop @serr @(Scoped_ (SystemProcess !! err)) .
  withSystemProcess_ .
  raiseUnder .
  queues @err options

pscope ::
  ∀ serr i o param proc err r .
  Members [Scoped proc (SystemProcess !! err) !! serr, Stop serr] r =>
  Members [ProcessInput i, ProcessOutput 'Stdout o, ProcessOutput 'Stderr o, Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  (param -> Sem r proc) ->
  param ->
  InterpretersFor (ScopeEffects i o err) r
pscope options consResource param sem =
  consResource param >>= \ proc ->
    restop @serr @(Scoped proc (SystemProcess !! err)) $
    withSystemProcess @proc proc $
    raiseUnder $
    queues @err options sem

-- |Interpret 'Process' with a system process resource whose file descriptors are connected to three 'TBMQueue's,
-- deferring decoding of stdout and stderr to the interpreters of two 'ProcessOutput' effects.
-- This variant:
-- - Models a daemon process that is not expected to terminate, causing 'Stop' to be sent to the scope callsite instead
--   of individual 'Process' actions.
-- - Is for parameterized scopes, meaning that a value of arbitrary type may be passed to
--   'Polysemy.Process.withProcessOneshotParam' which is then passed to the supplied function to produce a 'SysProcConf'
--   for the native process.
interpretProcess ::
  ∀ param proc i o r .
  Members (ProcessIO i o) r =>
  Member (Scoped proc (SystemProcess !! SystemProcessError) !! SystemProcessScopeError) r =>
  Members [Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  (param -> Sem (Stop ProcessError : r) proc) ->
  InterpreterFor (Scoped param (Process i o) !! ProcessError) r
interpretProcess options proc =
  interpretScopedResumableWith_ @(ScopeEffects i o SystemProcessError) acq (handleProcessWithQueues terminated)
  where
    acq ::
      ∀ e a .
      param ->
      Sem (ScopeEffects i o SystemProcessError ++ Stop ProcessError : Opaque e : r) a ->
      Sem (Stop ProcessError : Opaque e : r) a
    acq p sem =
      mapStop ProcessError.StartFailed do
        pscope @SystemProcessScopeError options (raise . raiseUnder . proc) p (insertAt @4 sem)

-- |Interpret 'Process' with a system process resource whose file descriptors are connected to three 'TBMQueue's,
-- deferring decoding of stdout and stderr to the interpreters of two 'ProcessOutput' effects.
-- This variant:
-- - Models a daemon process that is not expected to terminate, causing 'Stop' to be sent to the scope callsite instead
--   of individual 'Process' actions.
-- - Defers process config to 'SystemProcess'.
interpretProcess_ ::
  ∀ i o r .
  Member (Scoped_ (SystemProcess !! SystemProcessError) !! SystemProcessScopeError) r =>
  Members [ProcessOutput 'Stdout o, ProcessOutput 'Stderr o, ProcessInput i, Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  InterpreterFor (Scoped_ (Process i o) !! ProcessError) r
interpretProcess_ options =
  interpretScopedResumableWith_ @(ScopeEffects i o SystemProcessError) acq (handleProcessWithQueues terminated)
  where
    acq ::
      () ->
      Sem (ScopeEffects i o SystemProcessError ++ Stop ProcessError : Opaque e : r) a ->
      Sem (Stop ProcessError : Opaque e : r) a
    acq () sem =
      mapStop ProcessError.StartFailed do
        scope @SystemProcessScopeError options (insertAt @4 sem)

-- |Interpret 'Process' as a native 'Polysemy.Process.SystemProcess'.
-- This variant:
-- - Models a daemon process that is not expected to terminate, causing 'Stop' to be sent to the scope callsite instead
--   of individual 'Process' actions.
-- - Is for parameterized scopes, meaning that a value of arbitrary type may be passed to
--   'Polysemy.Process.withProcessOneshotParam' which is then passed to the supplied function to produce a 'SysProcConf'
--   for the native process.
interpretProcessNative ::
  ∀ param i o r .
  Members (ProcessIO i o) r =>
  Members [Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  (param -> Sem r (Either Text SysProcConf)) ->
  InterpreterFor (Scoped param (Process i o) !! ProcessError) r
interpretProcessNative options proc =
  interpretSystemProcessNative pure .
  interpretProcess options (insertAt @0 . proc) .
  raiseUnder

-- |Interpret 'Process' as a native 'Polysemy.Process.SystemProcess'.
-- This variant:
-- - Models a daemon process that is not expected to terminate, causing 'Stop' to be sent to the scope callsite instead
--   of individual 'Process' actions.
-- - Defers process config to 'SystemProcess'.
interpretProcessNative_ ::
  ∀ i o r .
  Members (ProcessIO i o) r =>
  Members [Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  SysProcConf ->
  InterpreterFor (Scoped_ (Process i o) !! ProcessError) r
interpretProcessNative_ options conf =
  interpretSystemProcessNative_ conf .
  interpretProcess_ options .
  raiseUnder

-- |Reinterpret 'Input' and 'Output' as 'Process'.
interpretInputOutputProcess ::
  ∀ i o r .
  Member (Process i o) r =>
  InterpretersFor [Input o, Output i] r
interpretInputOutputProcess =
  runOutputSem (Process.send @i @o) .
  runInputSem (Process.recv @i @o)

-- |Interpret 'Input ByteString' by polling a 'Handle' and stopping with 'ProcessError' when it fails.
interpretInputHandleBuffered ::
  Member (Embed IO) r =>
  Handle ->
  InterpreterFor (Input ByteString !! ProcessError) r
interpretInputHandleBuffered handle =
  interpretResumable \case
    Input ->
      stopNote (Unknown "handle closed") =<< tryMaybe (hGetSome handle 4096)

-- |Interpret 'Input ByteString' by polling a 'Handle' and stopping with 'ProcessError' when it fails.
-- This variant deactivates buffering for the 'Handle'.
interpretInputHandle ::
  Member (Embed IO) r =>
  Handle ->
  InterpreterFor (Input ByteString !! ProcessError) r
interpretInputHandle handle sem = do
  tryAny_ (hSetBuffering handle NoBuffering)
  interpretInputHandleBuffered handle sem

-- |Interpret 'Output ByteString' by writing to a 'Handle' and stopping with 'ProcessError' when it fails.
interpretOutputHandleBuffered ::
  Member (Embed IO) r =>
  Handle ->
  InterpreterFor (Output ByteString !! ProcessError) r
interpretOutputHandleBuffered handle =
  interpretResumable \case
    Output o ->
      stopNote (Unknown "handle closed") =<< tryMaybe (hPut handle o)

-- |Interpret 'Output ByteString' by writing to a 'Handle' and stopping with 'ProcessError' when it fails.
-- This variant deactivates buffering for the 'Handle'.
interpretOutputHandle ::
  Member (Embed IO) r =>
  Handle ->
  InterpreterFor (Output ByteString !! ProcessError) r
interpretOutputHandle handle sem = do
  tryAny_ (hSetBuffering handle NoBuffering)
  interpretOutputHandleBuffered handle sem

-- |Interpret 'Process' in terms of 'Input' and 'Output'.
-- Since the @i@ and @o@ parameters correspond to the abstraction of stdio fds of an external system process, @i@ is
-- written by 'Output' and @o@ is read from 'Input'.
-- This is useful to abstract the current process's stdio as an external process, with input and output swapped.
interpretProcessIO ::
  ∀ i o ie oe r .
  Members [Input ByteString !! ie, Output ByteString !! oe] r =>
  Members [ProcessInput i, ProcessOutput 'Stdout o, Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  InterpreterFor (Process i o !! ProcessError) r
interpretProcessIO (ProcessOptions discard qSize _) =
  withQueues @i @o qSize .
  withAsync_ (outputQueue @'Stdout @o @ie @(Input ByteString) discard (pure False) input) .
  withAsync_ (inputQueue @i @oe @(Output ByteString) output) .
  interpretResumable (handleProcessWithQueues (stop . Unknown)) .
  raiseUnder2

-- |Interpret 'Process' in terms of two 'Handle's.
-- This is useful to abstract the current process's stdio as an external process, with input and output swapped.
-- The first 'Handle' argument corresponds to the @o@ parameter, the second one to @i@, despite the first one usually
-- being the current process's stdin.
-- This is due to 'Process' abstracting an external process to whose stdin would be /written/, while the current one's
-- is /read/.
interpretProcessHandles ::
  ∀ i o r .
  Members [ProcessInput i, ProcessOutput 'Stdout o, Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  Handle ->
  Handle ->
  InterpreterFor (Process i o !! ProcessError) r
interpretProcessHandles options hIn hOut =
  interpretOutputHandle hOut .
  interpretInputHandle hIn .
  interpretProcessIO @i @o @ProcessError @ProcessError options .
  raiseUnder2

-- |Interpret 'Process' using the current process's stdin and stdout.
-- This mirrors the usual abstraction of an external process, to whose stdin would be /written/, while the current one's
-- is /read/.
interpretProcessCurrent ::
  Members [ProcessInput i, ProcessOutput 'Stdout o, Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  InterpreterFor (Process i o !! ProcessError) r
interpretProcessCurrent options =
  interpretProcessHandles options stdin stdout
