{-# options_haddock prune #-}
-- |Description: Process Interpreters for stdpipes, Internal
module Polysemy.Process.Interpreter.ProcessIOE where

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
import Polysemy.Process.Interpreter.Process (interpretProcessNative)

newtype In a =
  In { unIn :: a }
  deriving (Eq, Show)

newtype Out a =
  Out { unOut :: a }
  deriving (Eq, Show)

newtype Err a =
  Err { unErr :: a }
  deriving (Eq, Show)

data ProcessQueues =
  ProcessQueues {
    pqIn :: TBMQueue (In ByteString),
    pqOut :: TBMQueue (Out ByteString),
    pqErr :: TBMQueue (Err ByteString)
  }

processWithQueues :: ProcessConfig () () () -> ProcessConfig Handle Handle Handle
processWithQueues =
  setStdin createPipe . setStdout createPipe . setStderr createPipe

readQueue ::
  ∀ r .
  Members [Queue (In ByteString), Embed IO] r =>
  Bool ->
  Handle ->
  Sem r ()
readQueue discardWhenFull handle = do
  embed @IO (hSetBuffering handle NoBuffering)
  tryAny (hGetSome handle 4096) >>= traverse_ \ msg -> do
      if discardWhenFull then void (Queue.tryWrite (In msg)) else Queue.write (In msg)
      readQueue discardWhenFull handle

writeQueue ::
  ∀ r .
  Members [Queue (Out ByteString), Embed IO] r =>
  Handle ->
  Sem r ()
writeQueue handle = do
  embed @IO (hSetBuffering handle NoBuffering)
  spin
  where
    spin =
      Queue.read >>= \case
        QueueResult.Success (Out msg) ->
          traverse_ (const spin) =<< tryAny (hPut handle msg)
        _ ->
          pass

interpretQueues ::
  Members [Resource, Race, Embed IO] r =>
  ProcessQueues ->
  InterpretersFor [Queue (In ByteString), Queue (Out ByteString), Queue (Err ByteString)] r
interpretQueues (ProcessQueues inQ outQ errQ) =
  interpretQueueTBMWith errQ .
  interpretQueueTBMWith outQ .
  interpretQueueTBMWith inQ

interpretProcessWithQueues ::
  Members [Queue (In ByteString), Queue (Out ByteString), Queue (Err ByteString)] r =>
  InterpreterFor (Process ByteString ByteString ByteString !! ProcessError) r
interpretProcessWithQueues =
  interpretResumable \case
    Process.Recv ->
      Queue.read >>= \case
        QueueResult.Closed ->
          stop (Terminated "closed")
        QueueResult.NotAvailable ->
          stop (Terminated "impossible: empty")
        QueueResult.Success (In msg) ->
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
      whenM (Queue.closed @(Out ByteString)) (stop (Terminated "closed"))
      Queue.write (Out msg)

withSTMResources ::
  ∀ r a .
  Members [Resource, Embed IO] r =>
  Int ->
  (ProcessQueues -> Sem r a) ->
  Sem r a
withSTMResources qSize action = do
  withTBMQueue qSize \ inQ ->
    withTBMQueue qSize \ outQ ->
      withTBMQueue qSize \ errQ ->
        action (ProcessQueues inQ outQ errQ)

withProcessResources ::
  Members [Resource, Race, Async, Embed IO] r =>
  Bool ->
  Int ->
  System.Process Handle Handle Handle ->
  (ProcessQueues -> Sem r a) ->
  Sem r a
withProcessResources discardWhenFull qSize prc f =
  withSTMResources qSize \ qs ->
    interpretQueues qs $
    withAsync_ (readQueue discardWhenFull (getStderr prc)) $
    withAsync_ (readQueue discardWhenFull (getStdout prc)) $
    withAsync_ (writeQueue (getStdin prc)) $
    insertAt @0 (f qs)

interpretProcessQueues ::
  Members [Resource, Race, Async, Embed IO] r =>
  ProcessQueues ->
  InterpreterFor (Process ByteString ByteString ByteString !! ProcessError) r
interpretProcessQueues qs =
  interpretQueues qs .
  interpretProcessWithQueues .
  raiseUnder3

-- |Interpret 'Process' with a system process resource whose file descriptors are connected to three 'TBMQueue's,
-- producing 'ByteString's.
interpretProcessIOE ::
  Members [Resource, Race, Async, Embed IO] r =>
  Bool ->
  Int ->
  ProcessConfig () () () ->
  InterpreterFor (Scoped ProcessQueues (Process ByteString ByteString ByteString !! ProcessError)) r
interpretProcessIOE discardWhenFull qSize config =
  interpretProcessNative (processWithQueues config) (withProcessResources discardWhenFull qSize) interpretProcessQueues
