{-# options_haddock prune #-}
{-# language CPP #-}

-- |Description: Process Interpreters for stdpipes, Internal
module Polysemy.Process.Interpreter.ProcessStd where

import Control.Concurrent.STM.TBMQueue (TBMQueue)
import Data.ByteString (hGetSome, hPut)
import Polysemy (InterpretersFor, insertAt)
import Polysemy.Async (Async)
import Polysemy.Conc.Async (withAsync_)
import qualified Polysemy.Conc.Data.QueueResult as QueueResult
import qualified Polysemy.Conc.Effect.Queue as Queue
import Polysemy.Conc.Effect.Queue (Queue)
import Polysemy.Conc.Effect.Race (Race)
import Polysemy.Conc.Effect.Scoped (Scoped)
import Polysemy.Conc.Interpreter.Queue.TBM (interpretQueueTBMWith, withTBMQueue)
import Polysemy.Resource (Resource)
import Polysemy.Resume (interpretResumable, stop, type (!!))
import Prelude hiding (fromException)
import qualified System.Process.Typed as System
import System.Process.Typed (
  ProcessConfig,
  createPipe,
  getStderr,
  getStdin,
  getStdout,
  setStderr,
  setStdin,
  setStdout,
  )

import Polysemy.Process.Data.ProcessError (ProcessError (Terminated))
import qualified Polysemy.Process.Effect.Process as Process
import Polysemy.Process.Effect.Process (Process)
import qualified Polysemy.Process.Effect.ProcessOutput as ProcessOutput
import Polysemy.Process.Effect.ProcessOutput (ProcessOutput)
import Polysemy.Process.Interpreter.Process (interpretProcessNative)
import Polysemy.Process.Interpreter.ProcessOutput (
  interpretProcessOutputId,
  interpretProcessOutputLines,
  interpretProcessOutputText,
  interpretProcessOutputTextLines,
  )

#if !MIN_VERSION_relude(1,0,0)
import System.IO (BufferMode (NoBuffering), hSetBuffering)
#endif

newtype In a =
  In { unIn :: a }
  deriving (Eq, Show)

newtype Out a =
  Out { unOut :: a }
  deriving (Eq, Show)

newtype Err a =
  Err { unErr :: a }
  deriving (Eq, Show)

data ProcessQueues o e =
  ProcessQueues {
    pqIn :: TBMQueue (In ByteString),
    pqOut :: TBMQueue (Out o),
    pqErr :: TBMQueue (Err e)
  }

processWithQueues :: ProcessConfig () () () -> ProcessConfig Handle Handle Handle
processWithQueues =
  setStdin createPipe . setStdout createPipe . setStderr createPipe

outputQueue ::
  ∀ pipe chunk r .
  Coercible (pipe chunk) chunk =>
  Members [ProcessOutput chunk, Queue (pipe chunk), Embed IO] r =>
  Bool ->
  Handle ->
  Sem r ()
outputQueue discardWhenFull handle =
  spin ""
  where
    spin buffer = do
      void $ tryAny (hSetBuffering handle NoBuffering)
      tryAny (hGetSome handle 4096) >>= traverse_ \ msg -> do
        (chunks, newBuffer) <- ProcessOutput.chunk buffer msg
        for_ chunks \ (coerce @chunk @(pipe chunk) -> c) ->
          if discardWhenFull then void (Queue.tryWrite c) else Queue.write c
        spin newBuffer

inputQueue ::
  ∀ r .
  Members [Queue (In ByteString), Embed IO] r =>
  Handle ->
  Sem r ()
inputQueue handle = do
  void $ tryAny (hSetBuffering handle NoBuffering)
  spin
  where
    spin =
      Queue.read >>= \case
        QueueResult.Success (In msg) ->
          traverse_ (const spin) =<< tryAny (hPut handle msg)
        _ ->
          pass

interpretQueues ::
  Members [Resource, Race, Embed IO] r =>
  ProcessQueues o e ->
  InterpretersFor [Queue (In ByteString), Queue (Out o), Queue (Err e)] r
interpretQueues (ProcessQueues inQ outQ errQ) =
  interpretQueueTBMWith errQ .
  interpretQueueTBMWith outQ .
  interpretQueueTBMWith inQ

interpretProcessWithQueues ::
  ∀ o e r .
  Members [Queue (In ByteString), Queue (Out o), Queue (Err e)] r =>
  InterpreterFor (Process ByteString o e !! ProcessError) r
interpretProcessWithQueues =
  interpretResumable \case
    Process.Recv ->
      Queue.read >>= \case
        QueueResult.Closed ->
          stop (Terminated "closed")
        QueueResult.NotAvailable ->
          stop (Terminated "impossible: empty")
        QueueResult.Success (Out msg) ->
          pure msg
    Process.RecvError ->
      Queue.read >>= \case
        QueueResult.Closed ->
          stop (Terminated "closed")
        QueueResult.NotAvailable ->
          stop (Terminated "impossible: empty")
        QueueResult.Success (Err msg) ->
          pure msg
    Process.Send msg -> do
      whenM (Queue.closed @(In ByteString)) (stop (Terminated "closed"))
      Queue.write (In msg)

withSTMResources ::
  ∀ o e r a .
  Members [Resource, Embed IO] r =>
  Int ->
  (ProcessQueues o e -> Sem r a) ->
  Sem r a
withSTMResources qSize action = do
  withTBMQueue qSize \ inQ ->
    withTBMQueue qSize \ outQ ->
      withTBMQueue qSize \ errQ ->
        action (ProcessQueues inQ outQ errQ)

withProcessResources ::
  ∀ o e r a .
  Members [ProcessOutput e, ProcessOutput o, Resource, Race, Async, Embed IO] r =>
  Bool ->
  Int ->
  System.Process Handle Handle Handle ->
  (ProcessQueues o e -> Sem r a) ->
  Sem r a
withProcessResources discardWhenFull qSize prc f =
  withSTMResources qSize \ qs ->
    interpretQueues qs $
    withAsync_ (outputQueue @Err @e discardWhenFull (getStderr prc)) $
    withAsync_ (outputQueue @Out @o discardWhenFull (getStdout prc)) $
    withAsync_ (inputQueue (getStdin prc)) $
    insertAt @0 (f qs)

interpretProcessQueues ::
  Members [Resource, Race, Async, Embed IO] r =>
  ProcessQueues o e ->
  InterpreterFor (Process ByteString o e !! ProcessError) r
interpretProcessQueues qs =
  interpretQueues qs .
  interpretProcessWithQueues .
  raiseUnder3

-- |Interpret 'Process' with a system process resource whose file descriptors are connected to three 'TBMQueue's,
-- producing custom types with chunking.
-- The supplied functions will be used to decide whether chunks should be emitted after reading.
interpretProcessChunked ::
  Members [ProcessOutput e, ProcessOutput o, Resource, Race, Async, Embed IO] r =>
  -- |Whether to discard output chunks if the queue is full.
  Bool ->
  -- |Maximum number of chunks allowed to be queued in each of the three standard pipes.
  Int ->
  ProcessConfig () () () ->
  InterpreterFor (Scoped (ProcessQueues o e) (Process ByteString o e !! ProcessError)) r
interpretProcessChunked discardWhenFull qSize config =
  interpretProcessNative (processWithQueues config) (withProcessResources discardWhenFull qSize) interpretProcessQueues

-- |Interpret 'Process' with a system process resource whose file descriptors are connected to three 'TBMQueue's,
-- producing 'ByteString's.
interpretProcessByteString ::
  Members [Resource, Race, Async, Embed IO] r =>
  -- |Whether to discard output chunks if the queue is full.
  Bool ->
  -- |Maximum number of chunks allowed to be queued in each of the three standard pipes.
  Int ->
  ProcessConfig () () () ->
  InterpreterFor (Scoped (ProcessQueues ByteString ByteString) (Process ByteString ByteString ByteString !! ProcessError)) r
interpretProcessByteString discardWhenFull qSize config =
  interpretProcessOutputId .
  interpretProcessChunked discardWhenFull qSize config .
  raiseUnder

-- |Interpret 'Process' with a system process resource whose file descriptors are connected to three 'TBMQueue's,
-- producing chunks of lines of 'ByteString's.
interpretProcessByteStringLines ::
  Members [Resource, Race, Async, Embed IO] r =>
  -- |Whether to discard output chunks if the queue is full.
  Bool ->
  -- |Maximum number of chunks allowed to be queued in each of the three standard pipes.
  Int ->
  ProcessConfig () () () ->
  InterpreterFor (Scoped (ProcessQueues ByteString ByteString) (Process ByteString ByteString ByteString !! ProcessError)) r
interpretProcessByteStringLines discardWhenFull qSize config =
  interpretProcessOutputLines .
  interpretProcessChunked discardWhenFull qSize config .
  raiseUnder

-- |Interpret 'Process' with a system process resource whose file descriptors are connected to three 'TBMQueue's,
-- producing 'Text's.
interpretProcessText ::
  Members [Resource, Race, Async, Embed IO] r =>
  -- |Whether to discard output chunks if the queue is full.
  Bool ->
  -- |Maximum number of chunks allowed to be queued in each of the three standard pipes.
  Int ->
  ProcessConfig () () () ->
  InterpreterFor (Scoped (ProcessQueues Text Text) (Process ByteString Text Text !! ProcessError)) r
interpretProcessText discardWhenFull qSize config =
  interpretProcessOutputText .
  interpretProcessChunked discardWhenFull qSize config .
  raiseUnder

-- |Interpret 'Process' with a system process resource whose file descriptors are connected to three 'TBMQueue's,
-- producing chunks of lines of 'Text's.
interpretProcessTextLines ::
  Members [Resource, Race, Async, Embed IO] r =>
  -- |Whether to discard output chunks if the queue is full.
  Bool ->
  -- |Maximum number of chunks allowed to be queued in each of the three standard pipes.
  Int ->
  ProcessConfig () () () ->
  InterpreterFor (Scoped (ProcessQueues Text Text) (Process ByteString Text Text !! ProcessError)) r
interpretProcessTextLines discardWhenFull qSize config =
  interpretProcessOutputTextLines .
  interpretProcessChunked discardWhenFull qSize config .
  raiseUnder
