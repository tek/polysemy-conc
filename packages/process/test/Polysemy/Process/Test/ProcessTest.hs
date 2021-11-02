{-# options_ghc -fplugin=Polysemy.Plugin #-}

module Polysemy.Process.Test.ProcessTest where

import qualified Data.ByteString as ByteString
import Polysemy.Async (asyncToIOFinal)
import Polysemy.Conc.Interpreter.Race (interpretRace)
import Polysemy.Resume (resuming)
import Polysemy.Test (UnitTest, runTestAuto, (===))
import qualified System.Process.Typed as Process
import System.Process.Typed (ProcessConfig)

import qualified Polysemy.Process.Effect.Process as Process
import Polysemy.Process.Effect.Process (withProcess)
import Polysemy.Process.Interpreter.ProcessIOE (interpretProcessIOE)

config :: ProcessConfig () () ()
config =
  Process.proc "cat" []

message :: ByteString
message =
  ByteString.replicate 10 120

test_process :: UnitTest
test_process =
  runTestAuto $ interpretRace $ asyncToIOFinal $ interpretProcessIOE True 10 config do
    withProcess do
      response <- resuming (pure . show) do
        Process.send message
        Process.recv
      message === response
