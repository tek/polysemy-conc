{-# options_ghc -fplugin=Polysemy.Plugin #-}

module Polysemy.Process.Test.ProcessTest where

import Polysemy.Conc.Interpreter.Race (interpretRace)
import Polysemy.Resume (resumeHoistError)
import Polysemy.Test (UnitTest, runTestAuto, (===))
import qualified System.Process.Typed as Process
import System.Process.Typed (ProcessConfig)

import Polysemy.Process.Data.ProcessError (ProcessError)
import qualified Polysemy.Process.Effect.Process as Process
import Polysemy.Process.Effect.Process (withProcess)
import Polysemy.Process.Interpreter.ProcessStdio (interpretProcessByteStringNative, interpretProcessTextLinesNative)

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
  runTestAuto $ interpretRace $ asyncToIOFinal $ interpretProcessByteStringNative True 10 config do
    response <- resumeHoistError @ProcessError show do
      withProcess do
        Process.send message
        Process.recv
    message === response

test_processLines :: UnitTest
test_processLines =
  runTestAuto $ interpretRace $ asyncToIOFinal $ interpretProcessTextLinesNative True 10 config do
    response <- resumeHoistError @ProcessError show do
      withProcess do
        Process.send message
        replicateM 4 Process.recv
    messageLines === response
