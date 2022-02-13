{-# options_haddock prune #-}

-- |The effect 'SystemProcess' is a low-level abstraction of a native system process.
module Polysemy.Process.SystemProcess (
  module Polysemy.Process.Effect.SystemProcess,
  module Polysemy.Process.Interpreter.SystemProcess,
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
  interpretSystemProcessNative,
  interpretSystemProcessNativeSingle,
  interpretSystemProcessWithProcess,
  )
