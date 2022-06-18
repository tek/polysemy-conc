{-# options_ghc -fplugin=Polysemy.Plugin #-}

module Polysemy.Process.Test.ProcessTest where

import qualified Data.ByteString as ByteString
import qualified Polysemy.Conc.Effect.Race as Conc (timeout)
import Polysemy.Conc.Effect.Scoped (Scoped)
import Polysemy.Conc.Interpreter.Race (interpretRace)
import qualified Polysemy.Conc.Race as Race
import Polysemy.Resume (resumeHoistAs, resumeHoistError, runStop)
import Polysemy.Test (UnitTest, assertLeft, runTestAuto, unitTest, (===))
import Polysemy.Time (MilliSeconds (MilliSeconds), Seconds (Seconds))
import qualified System.Process.Typed as Process
import System.Process.Typed (ProcessConfig)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (ignoreTest)

import Polysemy.Process.Data.ProcessError (ProcessError)
import Polysemy.Process.Data.ProcessKill (ProcessKill (KillNever))
import Polysemy.Process.Data.ProcessOptions (ProcessOptions (kill))
import Polysemy.Process.Data.ProcessOutputParseResult (ProcessOutputParseResult (Done, Partial))
import qualified Polysemy.Process.Effect.Process as Process
import Polysemy.Process.Effect.Process (withProcess, withProcessOneshot)
import Polysemy.Process.Interpreter.ProcessOneshotStdio (interpretProcessOneshotTextLinesNative)
import Polysemy.Process.Interpreter.ProcessOutput (parseMany)
import Polysemy.Process.Interpreter.ProcessStdio (interpretProcessByteStringNative, interpretProcessTextLinesNative)

config :: ProcessConfig () () ()
config =
  Process.proc "cat" []

messageLines :: [Text]
messageLines =
  replicate 4 "line"

message :: Text
message =
  unlines messageLines

test_process :: UnitTest
test_process =
  runTestAuto $ interpretRace $ asyncToIOFinal $ interpretProcessByteStringNative def config do
    response <- resumeHoistError @ProcessError @(Scoped _ _) show do
      withProcess do
        Process.send (encodeUtf8 message)
        Race.timeout_ (throw "timed out") (Seconds 5) Process.recv
    message === decodeUtf8 response

test_processLines :: UnitTest
test_processLines =
  runTestAuto $ interpretRace $ asyncToIOFinal $ interpretProcessTextLinesNative def config do
    response <- resumeHoistError @ProcessError @(Scoped _ _) show do
      withProcess do
        Process.send message
        Race.timeout_ (throw "timed out") (Seconds 5) (replicateM 4 Process.recv)
    messageLines === response

test_processKillNever :: UnitTest
test_processKillNever =
  runTestAuto $ interpretRace $ asyncToIOFinal $ interpretProcessTextLinesNative def { kill = KillNever } config do
    result <- resumeHoistError @ProcessError @(Scoped _ _) show do
      Conc.timeout unit (MilliSeconds 100) do
        withProcess do
          Process.send message
          Process.recv
    -- This does not succeed. It should be 'Left', but apparently the `timeout` causes the `SystemProcess` scope to stop
    -- the process and makes the right side terminate regularly.
    assertLeft () result

test_processIncremental :: UnitTest
test_processIncremental =
  runTestAuto do
    (Nothing, ([Right "aa", Right "bb"], "")) === first void (parseMany parse Nothing "aabb")
    let (c1, r1) = parseMany parse Nothing "aabbc"
    ([Right "aa", Right "bb"], "") === r1
    case ($ "c") <$> c1 of
      Just (Done a "") -> "cc" === a
      a -> fail ("not Done: " <> show a)
    let (c2, r2) = parseMany parse Nothing "a"
    ([], "") === r2
    case ($ "a") <$> c2 of
      Just (Done a "") -> "aa" === a
      a -> fail ("not Done: " <> show a)
    (Nothing, ([Right "aa"], "")) === first void (parseMany parse c2 "a")
  where
    parse b
      | ByteString.length b == 1 =
        Partial (parse . (b <>))
      | otherwise =
        Done (ByteString.take 2 b) (ByteString.drop 2 b)

test_processOneshot :: UnitTest
test_processOneshot =
  runTestAuto $ interpretRace $ asyncToIOFinal $ interpretProcessOneshotTextLinesNative def conf do
    num <- runStop @Int $ withProcessOneshot do
      Race.timeout_ (throw "timed out") (Seconds 5) do
        for_ @[] [1..5] \ i ->
          resumeHoistAs i Process.recv
        unit
    assertLeft 5 num
  where
    conf =
      Process.proc "echo" ["-n", toString message]

test_processAll :: TestTree
test_processAll =
  testGroup "process" [
    unitTest "read raw chunks" test_process,
    unitTest "read lines" test_processLines,
    ignoreTest (unitTest "don't kill the process at the end of the scope" test_processKillNever),
    unitTest "expect termination" test_processOneshot
  ]
