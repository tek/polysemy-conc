{-# options_ghc -fplugin=Polysemy.Plugin #-}

module Polysemy.Process.Test.ProcessTest where

import Polysemy.Async (asyncToIOFinal)
import Polysemy.Conc.Interpreter.Race (interpretRace)
import Polysemy.Resume (resuming)
import Polysemy.Test (UnitTest, runTestAuto, (===))
import qualified System.Process.Typed as Process
import System.Process.Typed (ProcessConfig)

import qualified Polysemy.Process.Effect.Process as Process
import Polysemy.Process.Effect.Process (withProcess)
import Polysemy.Process.Interpreter.ProcessStd (interpretProcessByteString, interpretProcessTextLines)

config :: ProcessConfig () () ()
config =
  Process.proc "cat" []

messageLines :: [Text]
messageLines =
  replicate 4 "line"

message :: ByteString
message =
  encodeUtf8 (unlines messageLines)

test_process :: UnitTest
test_process =
  runTestAuto $ interpretRace $ asyncToIOFinal $ interpretProcessByteString True 10 config do
    withProcess do
      response <- resuming (pure . show) do
        Process.send message
        Process.recv
      message === response

test_processLines :: UnitTest
test_processLines =
  runTestAuto $ interpretRace $ asyncToIOFinal $ interpretProcessTextLines True 10 config do
    withProcess do
      response <- resuming (pure . pure . show) do
        Process.send message
        replicateM 4 Process.recv
      messageLines === response
