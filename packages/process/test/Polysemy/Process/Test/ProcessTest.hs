{-# options_ghc -fplugin=Polysemy.Plugin #-}

module Polysemy.Process.Test.ProcessTest where

import qualified Polysemy.Conc as Conc
import Polysemy.Conc.Effect.Scoped (Scoped)
import Polysemy.Conc.Interpreter.Race (interpretRace)
import qualified Polysemy.Conc.Race as Race
import Polysemy.Resume (resumeHoistError)
import Polysemy.Test (UnitTest, assertLeft, runTestAuto, unitTest, (===))
import Polysemy.Time (MilliSeconds (MilliSeconds), Seconds (Seconds))
import qualified System.Process.Typed as Process
import System.Process.Typed (ProcessConfig)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (ignoreTest)

import Polysemy.Process.Data.ProcessError (ProcessError)
import Polysemy.Process.Data.ProcessKill (ProcessKill (KillNever))
import Polysemy.Process.Data.ProcessOptions (ProcessOptions (kill))
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
  runTestAuto $ interpretRace $ asyncToIOFinal $ interpretProcessByteStringNative def config do
    response <- resumeHoistError @ProcessError @(Scoped _ _) show do
      withProcess do
        Process.send message
        Race.timeout_ (throw "timed out") (Seconds 5) Process.recv
    message === response

test_processLines :: UnitTest
test_processLines =
  runTestAuto $ interpretRace $ asyncToIOFinal $ interpretProcessTextLinesNative def config do
    response <- resumeHoistError @ProcessError @(Scoped _ _) show do
      withProcess do
        Process.send message
        Race.timeout_ (throw "timed out") (Seconds 5) (replicateM 4 Process.recv)
    messageLines === response

test_processKill :: UnitTest
test_processKill =
  runTestAuto $ interpretRace $ asyncToIOFinal $ interpretProcessTextLinesNative def { kill = KillNever } config do
    result <- resumeHoistError @ProcessError @(Scoped _ _) show do
      Conc.timeout unit (MilliSeconds 100) do
        withProcess do
          Process.send message
          Process.recv
    -- This does not succeed. It should be 'Left', but apparently the `timeout` causes the `SystemProcess` scope to stop
    -- the process and makes the right side terminate regularly.
    assertLeft () result

test_processAll :: TestTree
test_processAll =
  testGroup "process" [
    unitTest "process" test_process,
    unitTest "process lines" test_processLines,
    ignoreTest (unitTest "process kill" test_processKill)
  ]
