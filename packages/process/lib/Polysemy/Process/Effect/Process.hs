{-# options_haddock prune #-}

-- |Description: Process Effect, Internal
module Polysemy.Process.Effect.Process where

import Polysemy.Conc.Effect.Scoped (Scoped, scoped)
import Polysemy.Input (Input (Input))
import Polysemy.Output (Output (Output))
import Polysemy.Resume (interpretResumable, restop, type (!!))
import Prelude hiding (send)

-- |Abstraction of a process with input and output.
--
-- This effect is intended to be used in a scoped manner:
--
-- @
-- import Polysemy.Resume
-- import Polysemy.Conc
-- import Polysemy.Process
-- import qualified System.Process.Typed as System
--
-- prog :: Member (Scoped resource (Process Text Text !! err)) r => Sem r Text
-- prog =
--  resumeAs "failed" do
--    withProcess do
--      send "input"
--      recv
--
-- main :: IO ()
-- main = do
--   out <- runConc $ interpretProcessNative (System.proc "cat" []) prog
--   putStrLn out
-- @
data Process i o :: Effect where
  Recv :: Process i o m o
  Send :: i -> Process i o m ()

makeSem_ ''Process

-- |Obtain a chunk of output.
recv ::
  ∀ i o r .
  Member (Process i o) r =>
  Sem r o

-- |Send data to stdin.
send ::
  ∀ i o r .
  Member (Process i o) r =>
  i ->
  Sem r ()

-- |Create a scoped resource for 'Process'.
withProcess ::
  ∀ resource i o r .
  Member (Scoped resource (Process i o)) r =>
  InterpreterFor (Process i o) r
withProcess =
  scoped @resource

-- |Convert 'Output' and 'Input' to 'Process'.
runProcessIO ::
  ∀ i o err r .
  Member (Process i o !! err) r =>
  InterpretersFor [Output i !! err, Input o !! err] r
runProcessIO =
  interpretResumable \case
    Input ->
      restop @err @(Process i o) (recv @i @o)
  .
  interpretResumable \case
    Output o ->
      restop @err @(Process i o) (send @i @o o)
