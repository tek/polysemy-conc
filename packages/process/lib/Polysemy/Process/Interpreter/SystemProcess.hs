{-# options_haddock prune #-}
{-# language CPP #-}

-- |Description: SystemProcess Interpreters, Internal
module Polysemy.Process.Interpreter.SystemProcess where

import Data.ByteString (hGetSome, hPut)
import Polysemy.Conc.Effect.Scoped (Scoped)
import Polysemy.Conc.Interpreter.Scoped (runScoped)
import Polysemy.Resource (Resource, bracket)
import Polysemy.Resume (Stop, interpretResumable, stopNote, type (!!))
import Prelude hiding (fromException)
import qualified System.Posix as Signal
import System.Process (Pid, getPid)
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
  startProcess,
  stopProcess,
  unsafeProcessHandle,
  waitExitCode,
  )

import qualified Polysemy.Process.Data.SystemProcessError as SystemProcessError
import Polysemy.Process.Data.SystemProcessError (SystemProcessError)
import qualified Polysemy.Process.Effect.SystemProcess as SystemProcess
import Polysemy.Process.Effect.SystemProcess (SystemProcess)

#if !MIN_VERSION_relude(1,0,0)
import System.IO (BufferMode (NoBuffering), hSetBuffering)
#endif

type PipesProcess =
  System.Process Handle Handle Handle

processWithPipes :: ProcessConfig () () () -> ProcessConfig Handle Handle Handle
processWithPipes =
  setStdin createPipe .
  setStdout createPipe .
  setStderr createPipe

start ::
  Member (Embed IO) r =>
  ProcessConfig () () () ->
  Sem r PipesProcess
start =
  startProcess . processWithPipes

withProcess ::
  Members [Resource, Embed IO] r =>
  ProcessConfig () () () ->
  (PipesProcess -> Sem r a) ->
  Sem r a
withProcess config use =
  bracket (start config) (tryAny . stopProcess) \ p -> do
    unbuffer (getStdin p)
    unbuffer (getStdout p)
    unbuffer (getStderr p)
    use p
  where
    unbuffer h =
      void $ tryMaybe (hSetBuffering h NoBuffering)

terminate ::
  Member (Stop SystemProcessError) r =>
  Text ->
  Maybe a ->
  Sem r a
terminate msg =
  stopNote (SystemProcessError.Terminated msg)

tryStop ::
  Members [Stop SystemProcessError, Embed IO] r =>
  Text ->
  IO a ->
  Sem r a
tryStop msg =
  terminate msg <=< tryMaybe

processId ::
  Members [Stop SystemProcessError, Embed IO] r =>
  System.Process i o e ->
  Sem r Pid
processId process =
  terminate "getPid returned Nothing" =<< embed (getPid (unsafeProcessHandle process))

-- |Interpret 'SystemProcess' with a concrete 'System.Process' with connected pipes.
interpretSystemProcessWithProcess ::
  ∀ r .
  Member (Embed IO) r =>
  System.Process Handle Handle Handle ->
  InterpreterFor (SystemProcess !! SystemProcessError) r
interpretSystemProcessWithProcess process =
  interpretResumable \case
    SystemProcess.Pid ->
      processId process
    SystemProcess.Signal sig -> do
      pid <- processId process
      tryStop "signal failed" (Signal.signalProcess sig pid)
    SystemProcess.ReadStdout ->
      tryStop "stdout failed" (hGetSome (getStdout process) 4096)
    SystemProcess.ReadStderr ->
      tryStop "stderr failed" (hGetSome (getStderr process) 4096)
    SystemProcess.WriteStdin msg ->
      tryStop "stdin failed" (hPut (getStdin process) msg)
    SystemProcess.Wait ->
      tryStop "wait failed" (waitExitCode process)

-- |Interpret 'SystemProcess' as a single global 'System.Process' that's started immediately.
interpretSystemProcessNativeSingle ::
  ∀ r .
  Members [Resource, Embed IO] r =>
  ProcessConfig () () () ->
  InterpreterFor (SystemProcess !! SystemProcessError) r
interpretSystemProcessNativeSingle config sem =
  withProcess config \ process ->
    interpretSystemProcessWithProcess process sem

-- |Interpret 'SystemProcess' as a scoped 'System.Process' that's started wherever 'Polysemy.Process.withSystemProcess'
-- is called and terminated when the wrapped action finishes.
interpretSystemProcessNative ::
  ∀ r .
  Members [Resource, Embed IO] r =>
  ProcessConfig () () () ->
  InterpreterFor (Scoped PipesProcess (SystemProcess !! SystemProcessError)) r
interpretSystemProcessNative config =
  runScoped (withProcess config) interpretSystemProcessWithProcess
