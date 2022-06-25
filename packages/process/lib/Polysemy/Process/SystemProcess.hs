{-# options_haddock prune #-}

-- |The effect 'SystemProcess' is a low-level abstraction of a native system process.
module Polysemy.Process.SystemProcess (
  module Polysemy.Process.Effect.SystemProcess,
  module Polysemy.Process.Interpreter.SystemProcess,
  processConfig,
  shellConfig,
) where

import Path (Abs, File, Path, toFilePath)
import System.Process.Typed (proc, shell)

import Polysemy.Process.Effect.SystemProcess (
  SystemProcess (..),
  interrupt,
  pid,
  readStderr,
  readStdout,
  signal,
  wait,
  withSystemProcess,
  writeStdin,
  )
import Polysemy.Process.Interpreter.SystemProcess (
  SysProcConf,
  interpretSystemProcessNative,
  interpretSystemProcessNativeSingle,
  interpretSystemProcessWithProcess,
  )

processConfig :: Path Abs File -> [Text] -> SysProcConf
processConfig exe args =
  proc (toFilePath exe) (toString <$> args)
{-# inline processConfig #-}

shellConfig :: Text -> SysProcConf
shellConfig cmd =
  shell (toString cmd)
{-# inline shellConfig #-}
