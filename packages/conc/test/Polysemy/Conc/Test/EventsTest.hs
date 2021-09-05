module Polysemy.Conc.Test.EventsTest where

import Control.Concurrent.Chan.Unagi.Bounded (OutChan)
import Polysemy.Async (async, asyncToIOFinal, await)
import Polysemy.Test (UnitTest, assertJust, runTestAuto)

import qualified Polysemy.Conc.Effect.Events as Events
import qualified Polysemy.Conc.Effect.Sync as Sync
import Polysemy.Conc.Interpreter.Events (interpretEventsChan)
import Polysemy.Conc.Interpreter.Race (interpretRace)
import Polysemy.Conc.Interpreter.Sync (interpretSync)

test_events :: UnitTest
test_events =
  (runTestAuto . asyncToIOFinal) do
    interpretRace $ interpretSync @() $ interpretEventsChan @Int $ interpretEventsChan @Text do
      thread1 <- async do
        Events.subscribe @Int @(OutChan Int) do
          Events.consume @Int
      thread2 <- async do
        Events.subscribe @Int @(OutChan Int) do
          Events.consume @Int
      thread3 <- async do
        Events.subscribe @Text @(OutChan Text) do
          Sync.putBlock ()
          Events.consume @Text
      Sync.takeBlock @()
      Events.publish @(OutChan Text) ("test" :: Text)
      Events.publish @(OutChan Int) (1 :: Int)
      num1 <- await thread1
      num2 <- await thread2
      text1 <- await thread3
      assertJust @_ @IO 1 num1
      assertJust @_ @IO 1 num2
      assertJust @_ @IO "test" text1
