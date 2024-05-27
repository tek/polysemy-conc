{-# options_haddock prune #-}

-- | Description: SystemProcess Interpreters, Internal
module Polysemy.Process.Interpreter.SystemProcess where

import Data.ByteString (hGetSome, hPut)
import Polysemy.Resume (Stop, interpretResumable, interpretScopedR, stop, stopNote, stopTryIOError, type (!!))
import Prelude hiding (fromException)
import System.IO (BufferMode (NoBuffering), Handle, hSetBuffering)
import qualified System.Posix as Signal
import System.Process (Pid, getPid)
import System.Process.Typed (
  Process,
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
import Polysemy.Process.Data.SystemProcessError (SystemProcessError, SystemProcessScopeError (StartFailed))
import qualified Polysemy.Process.Effect.SystemProcess as SystemProcess
import Polysemy.Process.Effect.SystemProcess (SystemProcess)

-- | Convenience alias for a vanilla 'ProcessConfig', which will usually be transformed by interpreters to use 'Handle's.
type SysProcConf =
  ProcessConfig () () ()

-- | Convenience alias for the 'Process' type used by native interpreters.
type PipesProcess =
  Process Handle Handle Handle

processWithPipes :: SysProcConf -> ProcessConfig Handle Handle Handle
processWithPipes =
  setStdin createPipe .
  setStdout createPipe .
  setStderr createPipe

start ::
  Members [Stop SystemProcessScopeError, Embed IO] r =>
  SysProcConf ->
  Sem r PipesProcess
start =
  stopTryIOError SystemProcessError.StartFailed .
  startProcess .
  processWithPipes

withProcess ::
  Members [Resource, Stop SystemProcessScopeError, Embed IO] r =>
  SysProcConf ->
  (PipesProcess -> Sem r a) ->
  Sem r a
withProcess config use =
  bracket (start config) (tryIOError . stopProcess) \ p -> do
    unbuffer (getStdin p)
    unbuffer (getStdout p)
    unbuffer (getStderr p)
    use p
  where
    unbuffer h =
      void $ tryMaybe (hSetBuffering h NoBuffering)

withProcessOpaque ::
  Members [Resource, Embed IO] r =>
  ProcessConfig i o e ->
  (Process i o e -> Sem r a) ->
  Sem r a
withProcessOpaque config =
  bracket (startProcess config) (tryIOError . stopProcess)

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
  Process i o e ->
  Sem r Pid
processId process =
  terminate "getPid returned Nothing" =<< embed (getPid (unsafeProcessHandle process))

checkEof ::
  Member (Stop SystemProcessError) r =>
  ByteString ->
  Sem r ByteString
checkEof = \case
  "" ->
    stop (SystemProcessError.Terminated "Process terminated, empty ByteString read from handle")
  b ->
    pure b

-- | Handle 'SystemProcess' with a concrete 'System.Process' with connected pipes.
handleSystemProcessWithProcess ::
  ∀ r r0 a .
  Members [Stop SystemProcessError, Embed IO] r =>
  Process Handle Handle Handle ->
  SystemProcess (Sem r0) a ->
  Sem r a
handleSystemProcessWithProcess process = \case
  SystemProcess.Pid ->
    fromIntegral <$> processId process
  SystemProcess.Signal sig -> do
    pid <- processId process
    tryStop "signal failed" (Signal.signalProcess sig pid)
  SystemProcess.ReadStdout ->
    checkEof =<< tryStop "stdout failed" (hGetSome (getStdout process) 4096)
  SystemProcess.ReadStderr ->
    checkEof =<< tryStop "stderr failed" (hGetSome (getStderr process) 4096)
  SystemProcess.WriteStdin msg ->
    tryStop "stdin failed" (hPut (getStdin process) msg)
  SystemProcess.Wait ->
    tryStop "wait failed" (waitExitCode process)

-- | Interpret 'SystemProcess' with a concrete 'System.Process' with connected pipes.
interpretSystemProcessWithProcess ::
  ∀ r .
  Member (Embed IO) r =>
  Process Handle Handle Handle ->
  InterpreterFor (SystemProcess !! SystemProcessError) r
interpretSystemProcessWithProcess process =
  interpretResumable (handleSystemProcessWithProcess process)

-- | Interpret 'SystemProcess' as a single global 'System.Process' that's started immediately.
interpretSystemProcessNativeSingle ::
  ∀ r .
  Members [Stop SystemProcessScopeError, Resource, Embed IO] r =>
  SysProcConf ->
  InterpreterFor (SystemProcess !! SystemProcessError) r
interpretSystemProcessNativeSingle config sem =
  withProcess config \ process ->
    interpretSystemProcessWithProcess process sem

withProcConf ::
  Members [Stop SystemProcessScopeError, Resource, Embed IO] r =>
  (PipesProcess -> Sem r a) ->
  Either Text SysProcConf ->
  Sem r a
withProcConf use = \case
  Right conf ->
    withProcess conf use
  Left err ->
    stop (StartFailed err)
{-# inline withProcConf #-}

-- | Interpret 'SystemProcess' as a scoped 'System.Process' that's started wherever 'Polysemy.Process.withSystemProcess'
-- is called and terminated when the wrapped action finishes.
-- This variant is for parameterized scopes, allowing the consumer to supply a value of type @param@ to create the
-- process config.
interpretSystemProcessNative ::
  ∀ param r .
  Members [Resource, Embed IO] r =>
  (param -> Sem r (Either Text SysProcConf)) ->
  InterpreterFor (Scoped param (SystemProcess !! SystemProcessError) !! SystemProcessScopeError) r
interpretSystemProcessNative config =
  interpretScopedR (\ p u -> raise (raise (config p)) >>= withProcConf u) handleSystemProcessWithProcess

-- | Interpret 'SystemProcess' as a scoped 'System.Process' that's started wherever 'Polysemy.Process.withSystemProcess'
-- is called and terminated when the wrapped action finishes.
-- This variant takes a static 'SysProcConf'.
interpretSystemProcessNative_ ::
  ∀ r .
  Members [Resource, Embed IO] r =>
  SysProcConf ->
  InterpreterFor (Scoped_ (SystemProcess !! SystemProcessError) !! SystemProcessScopeError) r
interpretSystemProcessNative_ config =
  interpretScopedR (const (withProcess config)) handleSystemProcessWithProcess

-- | Interpret 'SystemProcess' with a concrete 'System.Process' with no connection to stdio.
interpretSystemProcessWithProcessOpaque ::
  ∀ i o e r .
  Member (Embed IO) r =>
  Process i o e ->
  InterpreterFor (SystemProcess !! SystemProcessError) r
interpretSystemProcessWithProcessOpaque process =
  interpretResumable \case
    SystemProcess.Pid ->
      fromIntegral <$> processId process
    SystemProcess.Signal sig -> do
      pid <- processId process
      tryStop "signal failed" (Signal.signalProcess sig pid)
    SystemProcess.ReadStdout ->
      stop SystemProcessError.NoPipes
    SystemProcess.ReadStderr ->
      stop SystemProcessError.NoPipes
    SystemProcess.WriteStdin _ ->
      stop SystemProcessError.NoPipes
    SystemProcess.Wait ->
      tryStop "wait failed" (waitExitCode process)

-- | Interpret 'SystemProcess' as a single global 'System.Process' that's started immediately.
interpretSystemProcessNativeOpaqueSingle ::
  ∀ i o e r .
  Members [Resource, Embed IO] r =>
  ProcessConfig i o e ->
  InterpreterFor (SystemProcess !! SystemProcessError) r
interpretSystemProcessNativeOpaqueSingle config sem =
  withProcessOpaque config \ process ->
    interpretSystemProcessWithProcessOpaque process sem

-- | Interpret 'SystemProcess' as a scoped 'System.Process' that's started wherever 'Polysemy.Process.withSystemProcess'
-- is called and terminated when the wrapped action finishes.
interpretSystemProcessNativeOpaque ::
  ∀ i o e r .
  Members [Resource, Embed IO] r =>
  ProcessConfig i o e ->
  InterpreterFor (Scoped_ (SystemProcess !! SystemProcessError)) r
interpretSystemProcessNativeOpaque config =
  runScopedNew \ () -> interpretSystemProcessNativeOpaqueSingle config
