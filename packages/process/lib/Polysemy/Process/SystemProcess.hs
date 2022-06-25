{-# options_haddock prune #-}

-- |The effect 'SystemProcess' is a low-level abstraction of a native system process.
module Polysemy.Process.SystemProcess (
  module Polysemy.Process.Effect.SystemProcess,
  module Polysemy.Process.Interpreter.SystemProcess,
  module Polysemy.Process.SysProcConf,
) where

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
import Polysemy.Process.SysProcConf
