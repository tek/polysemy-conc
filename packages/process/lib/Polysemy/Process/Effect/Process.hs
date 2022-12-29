{-# options_haddock prune #-}

-- |Description: Process Effect, Internal
module Polysemy.Process.Effect.Process where

import Polysemy.Scoped (Scoped, Scoped_, scoped, scoped_)
import Polysemy.Input (Input (Input))
import Polysemy.Output (Output (Output))
import Polysemy.Resume (interpretResumable, restop, type (!!))
import Prelude hiding (send)

-- |Abstraction of a process with input and output.
--
-- This effect is intended to be used in a scoped_ manner:
--
-- @
-- import Polysemy.Resume
-- import Polysemy.Conc
-- import Polysemy.Process
-- import qualified System.Process.Typed as System
--
-- prog :: Member (Scoped_ (Process Text Text !! err)) r => Sem r Text
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

-- |Create a scoped_ resource for 'Process'.
-- The process configuration may depend on the provided value of type @param@.
-- This variant models daemon processes that are expected to run forever, with 'Polysemy.Resume.Stop' being sent to this
-- function, if at all.
withProcess ::
  ∀ param i o r .
  Member (Scoped param (Process i o)) r =>
  param ->
  InterpreterFor (Process i o) r
withProcess =
  scoped @param

-- |Create a scoped_ resource for 'Process'.
-- The process configuration may depend on the provided value of type @param@.
-- This variant models processes that are expected to terminate, with 'Polysemy.Resume.Stop' being sent to individual
-- actions within the scope.
withProcessOneshot ::
  ∀ param i o err r .
  Member (Scoped param (Process i o !! err)) r =>
  param ->
  InterpreterFor (Process i o !! err) r
withProcessOneshot =
  scoped @param

-- |Create a scoped_ resource for 'Process'.
-- The process configuration is provided to the interpreter statically.
-- This variant models daemon processes that are expected to run forever, with 'Polysemy.Resume.Stop' being sent to this
-- function, if at all.
withProcess_ ::
  ∀ i o r .
  Member (Scoped_ (Process i o)) r =>
  InterpreterFor (Process i o) r
withProcess_ =
  scoped_

-- |Create a scoped_ resource for 'Process'.
-- The process configuration is provided to the interpreter statically.
-- This variant models processes that are expected to terminate, with 'Polysemy.Resume.Stop' being sent to individual
-- actions within the scope.
withProcessOneshot_ ::
  ∀ i o err r .
  Member (Scoped_ (Process i o !! err)) r =>
  InterpreterFor (Process i o !! err) r
withProcessOneshot_ =
  scoped_

-- |Convert 'Output' and 'Input' to 'Process' for a daemon process.
runProcessIO ::
  ∀ i o r .
  Member (Process i o) r =>
  InterpretersFor [Output i, Input o] r
runProcessIO =
  interpret \case
    Input ->
      recv @i @o
  .
  interpret \case
    Output o ->
      send @i @o o

-- |Convert 'Output' and 'Input' to 'Process' for a oneshot process.
runProcessOneshotIO ::
  ∀ i o err r .
  Member (Process i o !! err) r =>
  InterpretersFor [Output i !! err, Input o !! err] r
runProcessOneshotIO =
  interpretResumable \case
    Input ->
      restop @err @(Process i o) (recv @i @o)
  .
  interpretResumable \case
    Output o ->
      restop @err @(Process i o) (send @i @o o)
