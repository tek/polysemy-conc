{-# options_haddock prune #-}

-- |Description: SystemProcess Effect, Internal
module Polysemy.Process.Effect.SystemProcess where

import Polysemy.Conc.Effect.PScoped (PScoped, pscoped)
import Polysemy.Conc.Effect.Scoped (Scoped, scoped)
import Polysemy.Resume (type (!!))
import System.Exit (ExitCode)
import qualified System.Posix as Signal
import System.Posix (Signal)

import Polysemy.Process.Data.Pid (Pid)

-- |Low-level interface for a process, operating on raw chunks of bytes.
-- Interface is modeled after "System.Process".
data SystemProcess :: Effect where
  -- |Read a chunk from stdout.
  ReadStdout :: SystemProcess m ByteString
  -- |Read a chunk from stderr.
  ReadStderr :: SystemProcess m ByteString
  -- |Write a 'ByteString' to stdin.
  WriteStdin :: ByteString -> SystemProcess m ()
  -- |Obtain the process ID.
  Pid :: SystemProcess m Pid
  -- |Send a 'System.Posix.Signal' to the process.
  Signal :: Signal -> SystemProcess m ()
  -- |Wait for the process to terminate, returning its exit code.
  Wait :: SystemProcess m ExitCode

makeSem ''SystemProcess

-- |Create a scoped resource for 'SystemProcess'.
-- The process configuration may depend on the provided value of type @param@.
withSystemProcess ::
  ∀ param resource err r .
  Member (PScoped param resource (SystemProcess !! err)) r =>
  param ->
  InterpreterFor (SystemProcess !! err) r
withSystemProcess =
  pscoped @param @resource

-- |Create a scoped resource for 'SystemProcess'.
-- The process configuration is provided to the interpreter statically.
withSystemProcess_ ::
  ∀ resource err r .
  Member (Scoped resource (SystemProcess !! err)) r =>
  InterpreterFor (SystemProcess !! err) r
withSystemProcess_ =
  scoped @resource

-- |Send signal INT(2) to the process.
interrupt ::
  Member SystemProcess r =>
  Sem r ()
interrupt =
  signal Signal.sigINT

-- |Send signal TERM(15) to the process.
term ::
  Member SystemProcess r =>
  Sem r ()
term =
  signal Signal.sigTERM

-- |Send signal KILL(9) to the process.
kill ::
  Member SystemProcess r =>
  Sem r ()
kill =
  signal Signal.sigKILL
