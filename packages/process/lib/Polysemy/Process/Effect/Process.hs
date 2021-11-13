{-# options_haddock prune #-}
-- |Description: Process Effect, Internal
module Polysemy.Process.Effect.Process where

import Polysemy (makeSem_)
import Polysemy.Conc.Effect.Scoped (Scoped, scoped)
import Polysemy.Resume (type (!!))

-- |Abstraction of a process with stdin/stdout/stderr.
--
-- This effect is intended to be used in a scoped manner:
--
-- @
-- import Polysemy.Resume
-- import Polysemy.Conc
-- import Polysemy.Process
-- import qualified System.Process.Typed as System
--
-- prog :: Member (Scoped resource (Process Text Text e !! err)) r => Sem r Text
-- prog =
--  withProcess do
--    resumeAs "failed" do
--      send "input"
--      recv
--
-- main :: IO ()
-- main = do
--   out <- runConc $ interpretProcessNative (System.proc "cat" []) prog
--   putStrLn out
-- @
data Process i o e :: Effect where
  Recv :: Process i o e m o
  RecvError :: Process i o e m e
  Send :: i -> Process i o e m ()

makeSem_ ''Process

-- |Obtain a chunk of stdout.
recv ::
  ∀ i o e r .
  Member (Process i o e) r =>
  Sem r o

-- |Obtain a chunk of stderr.
recvError ::
  ∀ i o e r .
  Member (Process i o e) r =>
  Sem r e

-- |Send data to stdin.
send ::
  ∀ i o e r .
  Member (Process i o e) r =>
  i ->
  Sem r ()

-- |Create a scoped resource for 'Process'.
withProcess ::
  ∀ resource i o e err r .
  Member (Scoped resource (Process i o e !! err)) r =>
  InterpreterFor (Process i o e !! err) r
withProcess =
  scoped @resource
