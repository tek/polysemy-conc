module Polysemy.Process.Effect.SystemProcess where

import Polysemy.Conc.Effect.Scoped (Scoped, scoped)
import Polysemy.Resume (type (!!))
import System.Exit (ExitCode)
import qualified System.Posix as Signal
import System.Posix (Signal)
import System.Process (Pid)

-- |Low-level interface for a process, operating on raw chunks of bytes.
-- Interface is modeled after "System.Process".
data SystemProcess :: Effect where
  ReadStdout :: SystemProcess m ByteString
  ReadStderr :: SystemProcess m ByteString
  WriteStdin :: ByteString -> SystemProcess m ()
  Pid :: SystemProcess m Pid
  Signal :: Signal -> SystemProcess m ()
  Wait :: SystemProcess m ExitCode

makeSem ''SystemProcess

-- |Create a scoped resource for 'SystemProcess'.
withSystemProcess ::
  ∀ resource err r .
  Member (Scoped resource (SystemProcess !! err)) r =>
  InterpreterFor (SystemProcess !! err) r
withSystemProcess =
  scoped @resource

-- |Send signal INT(2) to the process.
interrupt ::
  Member SystemProcess r =>
  Sem r ()
interrupt =
  signal Signal.sigINT

-- |Send signal INT(15) to the process.
term ::
  Member SystemProcess r =>
  Sem r ()
term =
  signal Signal.sigTERM

-- |Send signal INT(9) to the process.
kill ::
  Member SystemProcess r =>
  Sem r ()
kill =
  signal Signal.sigKILL
