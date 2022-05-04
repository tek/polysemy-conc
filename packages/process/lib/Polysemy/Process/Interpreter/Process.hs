{-# options_haddock prune #-}

-- |Description: Process Interpreters, Internal
module Polysemy.Process.Interpreter.Process where

import Control.Concurrent.STM.TBMQueue (TBMQueue)
import Data.ByteString (hGetSome, hPut)
import qualified Polysemy.Conc as Conc
import Polysemy.Conc.Async (withAsync_)
import qualified Polysemy.Conc.Data.QueueResult as QueueResult
import qualified Polysemy.Conc.Effect.Queue as Queue
import Polysemy.Conc.Effect.Queue (Queue)
import Polysemy.Conc.Effect.Race (Race)
import Polysemy.Conc.Effect.Scoped (Scoped)
import Polysemy.Conc.Interpreter.Queue.TBM (interpretQueueTBMWith, withTBMQueue)
import Polysemy.Conc.Interpreter.Scoped (interpretScopedResumableWith_)
import Polysemy.Input (Input (Input))
import Polysemy.Output (Output (Output))
import Polysemy.Resume (Stop, interpretResumable, resumeOr, resume_, stop, stopNote, type (!!))
import Prelude hiding (fromException)
import System.IO (BufferMode (NoBuffering), Handle, hSetBuffering, stdin, stdout)

import Polysemy.Process.Data.ProcessError (ProcessError (Terminated))
import Polysemy.Process.Data.ProcessKill (ProcessKill (KillAfter, KillImmediately, KillNever))
import Polysemy.Process.Data.ProcessOptions (ProcessOptions (ProcessOptions))
import qualified Polysemy.Process.Effect.Process as Process
import Polysemy.Process.Effect.Process (Process)
import qualified Polysemy.Process.Effect.ProcessInput as ProcessInput
import Polysemy.Process.Effect.ProcessInput (ProcessInput)
import qualified Polysemy.Process.Effect.ProcessOutput as ProcessOutput
import Polysemy.Process.Effect.ProcessOutput (OutputPipe (Stderr, Stdout), ProcessOutput)
import qualified Polysemy.Process.Effect.SystemProcess as SystemProcess
import Polysemy.Process.Effect.SystemProcess (SystemProcess, withSystemProcess)
import Polysemy.Process.Interpreter.ProcessInput (interpretProcessInputId, interpretProcessInputText)
import Polysemy.Process.Interpreter.ProcessOutput (
  interpretProcessOutputId,
  interpretProcessOutputIgnore,
  interpretProcessOutputLines,
  interpretProcessOutputText,
  interpretProcessOutputTextLines,
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
  Process i o m a ->
  Sem r a
handleProcessWithQueues = \case
  Process.Recv ->
    Queue.read >>= \case
      QueueResult.Closed ->
        stop (Terminated "closed")
      QueueResult.NotAvailable ->
        stop (Terminated "impossible: empty")
      QueueResult.Success (Out msg) ->
        pure msg
  Process.Send msg -> do
    whenM (Queue.closed @(In i)) (stop (Terminated "closed"))
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

outputQueue ::
  ∀ p chunk err eff r .
  Members [eff !! err, ProcessOutput p chunk, Queue (Out chunk), Embed IO] r =>
  Bool ->
  Sem (eff : r) ByteString ->
  Sem r ()
outputQueue discardWhenFull readChunk = do
  spin ""
  where
    spin buffer =
      resumeOr @err readChunk (write buffer) (const (Queue.close @(Out chunk)))
    write buffer msg = do
      (chunks, newBuffer) <- ProcessOutput.chunk @p @chunk buffer msg
      for_ chunks \ (Out -> c) ->
        if discardWhenFull then void (Queue.tryWrite c) else Queue.write c
      spin newBuffer

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
  [Queue (In i), Queue (Out o), SystemProcess !! err]

scope ::
  ∀ i o resource err r .
  Member (Scoped resource (SystemProcess !! err)) r =>
  Members [ProcessInput i, ProcessOutput 'Stdout o, ProcessOutput 'Stderr o, Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  InterpretersFor (ScopeEffects i o err) r
scope (ProcessOptions discard qSize kill) =
  withSystemProcess @resource .
  withQueues qSize .
  withAsync_ (outputQueue @'Stderr @o @err @SystemProcess discard SystemProcess.readStderr) .
  withAsync_ (outputQueue @'Stdout @o @err @SystemProcess discard SystemProcess.readStdout) .
  withAsync_ (inputQueue @i @err @SystemProcess SystemProcess.writeStdin) .
  withKill @err kill

-- |Interpret 'Process' with a system process resource whose file descriptors are connected to three 'TBMQueue's,
-- deferring decoding of stdout and stderr to the interpreters of two 'ProcessOutput' effects.
interpretProcess ::
  ∀ resource err i o r .
  Member (Scoped resource (SystemProcess !! err)) r =>
  Members [ProcessOutput 'Stdout o, ProcessOutput 'Stderr o, ProcessInput i, Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  InterpreterFor (Scoped () (Process i o) !! ProcessError) r
interpretProcess options =
  interpretScopedResumableWith_ @(ScopeEffects i o err) (scope @i @o @resource options) handleProcessWithQueues

-- |Interpret 'Process' with a system process resource whose stdin/stdout are connected to two 'TBMQueue's,
-- producing 'ByteString's.
-- Silently discards stderr.
interpretProcessByteString ::
  ∀ resource err r .
  Members [Scoped resource (SystemProcess !! err), Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  InterpreterFor (Scoped () (Process ByteString ByteString) !! ProcessError) r
interpretProcessByteString options =
  interpretProcessOutputIgnore @'Stderr @ByteString .
  interpretProcessOutputId @'Stdout .
  interpretProcessInputId .
  interpretProcess @resource @err options .
  raiseUnder3

-- |Interpret 'Process' with a system process resource whose stdin/stdout are connected to two 'TBMQueue's,
-- producing chunks of lines of 'ByteString's.
-- Silently discards stderr.
interpretProcessByteStringLines ::
  ∀ resource err r .
  Members [Scoped resource (SystemProcess !! err), Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  InterpreterFor (Scoped () (Process ByteString ByteString) !! ProcessError) r
interpretProcessByteStringLines options =
  interpretProcessOutputIgnore @'Stderr @ByteString .
  interpretProcessOutputLines @'Stdout .
  interpretProcessInputId .
  interpretProcess @resource @err options .
  raiseUnder3

-- |Interpret 'Process' with a system process resource whose stdin/stdout are connected to two 'TBMQueue's,
-- producing 'Text's.
-- Silently discards stderr.
interpretProcessText ::
  ∀ resource err r .
  Members [Scoped resource (SystemProcess !! err), Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  InterpreterFor (Scoped () (Process Text Text) !! ProcessError) r
interpretProcessText options =
  interpretProcessOutputIgnore @'Stderr @Text .
  interpretProcessOutputText @'Stdout .
  interpretProcessInputText .
  interpretProcess @resource @err options .
  raiseUnder3

-- |Interpret 'Process' with a system process resource whose stdin/stdout are connected to two 'TBMQueue's,
-- producing chunks of lines of 'Text's.
-- Silently discards stderr.
interpretProcessTextLines ::
  ∀ resource err r .
  Members [Scoped resource (SystemProcess !! err), Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  InterpreterFor (Scoped () (Process Text Text) !! ProcessError) r
interpretProcessTextLines options =
  interpretProcessOutputIgnore @'Stderr @Text .
  interpretProcessOutputTextLines @'Stdout .
  interpretProcessInputText .
  interpretProcess @resource @err options .
  raiseUnder3

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
      stopNote (Terminated "handle closed") =<< tryMaybe (hGetSome handle 4096)

-- |Interpret 'Input ByteString' by polling a 'Handle' and stopping with 'ProcessError' when it fails.
-- This variant deactivates buffering for the 'Handle'.
interpretInputHandle ::
  Member (Embed IO) r =>
  Handle ->
  InterpreterFor (Input ByteString !! ProcessError) r
interpretInputHandle handle sem = do
  void $ tryMaybe (hSetBuffering handle NoBuffering)
  interpretInputHandleBuffered handle sem

-- |Interpret 'Output ByteString' by writing to a 'Handle' and stopping with 'ProcessError' when it fails.
interpretOutputHandleBuffered ::
  Member (Embed IO) r =>
  Handle ->
  InterpreterFor (Output ByteString !! ProcessError) r
interpretOutputHandleBuffered handle =
  interpretResumable \case
    Output o ->
      stopNote (Terminated "handle closed") =<< tryMaybe (hPut handle o)

-- |Interpret 'Output ByteString' by writing to a 'Handle' and stopping with 'ProcessError' when it fails.
-- This variant deactivates buffering for the 'Handle'.
interpretOutputHandle ::
  Member (Embed IO) r =>
  Handle ->
  InterpreterFor (Output ByteString !! ProcessError) r
interpretOutputHandle handle sem = do
  void $ tryMaybe (hSetBuffering handle NoBuffering)
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
  withAsync_ (outputQueue @'Stdout @o @ie @(Input ByteString) discard input) .
  withAsync_ (inputQueue @i @oe @(Output ByteString) output) .
  interpretResumable handleProcessWithQueues .
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
