{-# options_ghc -fplugin=Polysemy.Plugin #-}

module Polysemy.Process.Test.ProcessTest where

import Polysemy.Conc.Effect.Scoped (Scoped)
import Polysemy.Conc.Interpreter.Race (interpretRace)
import qualified Polysemy.Conc.Race as Race
import Polysemy.Resume (resumeHoistError)
import Polysemy.Test (UnitTest, runTestAuto, (===))
import Polysemy.Time (Seconds (Seconds))
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
    response <- resumeHoistError @ProcessError @(Scoped _ _) show do
      withProcess do
        Process.send message
        Race.timeout_ (throw "timed out") (Seconds 5) Process.recv
    message === response

test_processLines :: UnitTest
test_processLines =
  runTestAuto $ interpretRace $ asyncToIOFinal $ interpretProcessTextLinesNative True 10 config do
    response <- resumeHoistError @ProcessError @(Scoped _ _) show do
      withProcess do
        Process.send message
        Race.timeout_ (throw "timed out") (Seconds 5) (replicateM 4 Process.recv)
    messageLines === response
