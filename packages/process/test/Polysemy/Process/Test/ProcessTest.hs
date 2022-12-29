{-# options_ghc -fplugin=Polysemy.Plugin #-}

module Polysemy.Process.Test.ProcessTest where

import qualified Data.ByteString as ByteString
import qualified Polysemy.Conc.Effect.Race as Conc (timeout)
import Polysemy.Conc.Effect.Race (Race)
import Polysemy.Conc.Interpreter.Race (interpretRace)
import qualified Polysemy.Conc.Race as Race
import Polysemy.Resume (resumeEither, resumeHoistAs, resumeHoistError, resuming, runStop, type (!!), (<!))
import Polysemy.Test (TestError (TestError), UnitTest, assertJust, assertLeft, evalLeft, runTestAuto, unitTest, (===))
import Polysemy.Time (MilliSeconds (MilliSeconds), Seconds (Seconds))
import System.Exit (ExitCode (ExitSuccess))
import qualified System.Process.Typed as Process
import System.Process.Typed (ProcessConfig)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (ignoreTest)

import qualified Polysemy.Process.Data.ProcessError as ProcessError
import Polysemy.Process.Data.ProcessError (ProcessError)
import Polysemy.Process.Data.ProcessKill (ProcessKill (KillNever))
import Polysemy.Process.Data.ProcessOptions (ProcessOptions (kill))
import Polysemy.Process.Data.ProcessOutputParseResult (ProcessOutputParseResult (Done, Partial))
import Polysemy.Process.Data.SystemProcessError (SystemProcessScopeError (StartFailed))
import qualified Polysemy.Process.Effect.Process as Process
import Polysemy.Process.Effect.Process (Process, withProcessOneshot, withProcess_)
import qualified Polysemy.Process.Effect.SystemProcess as SystemProcess
import Polysemy.Process.Effect.SystemProcess (withSystemProcess_)
import Polysemy.Process.Interpreter.Process (interpretProcessNative_)
import Polysemy.Process.Interpreter.ProcessIO (ProcessIO, interpretProcessByteString, interpretProcessTextLines)
import Polysemy.Process.Interpreter.ProcessOneshot (interpretProcessOneshotNative)
import Polysemy.Process.Interpreter.ProcessOutput (parseMany)
import Polysemy.Process.Interpreter.SystemProcess (SysProcConf, interpretSystemProcessNative_)

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
  runTestAuto $ interpretRace $ asyncToIOFinal $ interpretProcessByteString $ interpretProcessNative_ def config do
    response <- resumeHoistError @ProcessError @(Scoped _ _) show do
      withProcess_ do
        Process.send (encodeUtf8 message)
        Race.timeout_ (throw "timed out") (Seconds 5) Process.recv
    message === decodeUtf8 response

test_processLines :: UnitTest
test_processLines =
  runTestAuto $ interpretRace $ asyncToIOFinal $ interpretProcessTextLines $ interpretProcessNative_ def config do
    response <- resumeHoistError @ProcessError @(Scoped _ _) show do
      withProcess_ do
        Process.send message
        Race.timeout_ (throw "timed out") (Seconds 5) (replicateM 4 Process.recv)
    messageLines === response

test_processKillNever :: UnitTest
test_processKillNever =
  runTestAuto $ interpretRace $ asyncToIOFinal $ interpretProcessTextLines $ interpretProcessNative_ def { kill = KillNever } config do
    result <- resumeHoistError @ProcessError @(Scoped _ _) show do
      Conc.timeout unit (MilliSeconds 100) do
        withProcess_ do
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

interpretOneshot ::
  Members [Error TestError, Resource, Race, Async, Embed IO] r =>
  (Text -> SysProcConf) ->
  InterpretersFor (Scoped Text (Process Text Text !! ProcessError) : ProcessIO Text Text) r
interpretOneshot conf =
  interpretProcessTextLines .
  interpretProcessOneshotNative def (pure . Right . conf) .
  resumeHoistError (TestError . show @Text @SystemProcessScopeError) .
  insertAt @1

test_processOneshot :: UnitTest
test_processOneshot =
  runTestAuto $ interpretRace $ asyncToIOFinal $ interpretOneshot conf do
    num <- runStop @Int $ withProcessOneshot message do
      Race.timeout_ (throw "timed out") (Seconds 5) do
        for_ @[] [1..5] \ i ->
          resumeHoistAs i Process.recv
        unit
    assertLeft 5 num
  where
    conf msg =
      Process.proc "echo" ["-n", toString msg]

test_exit :: UnitTest
test_exit =
  runTestAuto $ interpretRace $ asyncToIOFinal $ interpretProcessByteString $ interpretProcessNative_ def conf do
    response <- resuming @_ @(Scoped _ _) (pure . Just) $ withProcess_ do
      Race.timeout_ (throw (TestError "timed out")) (Seconds 5) do
        void Process.recv
        void Process.recv
      pure Nothing
    assertJust (ProcessError.Exit ExitSuccess) response
  where
    conf =
      Process.proc "echo" ["-n", "text"]

test_startFailed :: UnitTest
test_startFailed =
  runTestAuto $ interpretRace $ asyncToIOFinal $ interpretSystemProcessNative_ conf do
    result <- resumeEither $ withSystemProcess_ do
      Nothing <! (Just <$> SystemProcess.wait)
    evalLeft result >>= \case
      StartFailed _ ->
        unit
  where
    conf =
      Process.proc "fnord-detector" []

test_processAll :: TestTree
test_processAll =
  testGroup "process" [
    unitTest "read raw chunks" test_process,
    unitTest "read lines" test_processLines,
    ignoreTest (unitTest "don't kill the process at the end of the scope" test_processKillNever),
    unitTest "expect termination" test_processOneshot,
    unitTest "daemon exit code" test_exit,
    unitTest "system process start error" test_startFailed
  ]
