module Polysemy.Conc.Test.EventsTest where

import Control.Concurrent.Chan.Unagi.Bounded (OutChan)
import Polysemy.Test (UnitTest, assertJust, runTestAuto)

import qualified Polysemy.Conc.Effect.Events as Events
import qualified Polysemy.Conc.Effect.Sync as Sync
import Polysemy.Conc.Interpreter.Events (interpretEventsChan)
import Polysemy.Conc.Interpreter.Race (interpretRace)
import Polysemy.Conc.Interpreter.Sync (interpretSync)

test_events :: UnitTest
test_events =
  (runTestAuto . asyncToIOFinal) $
  interpretRace $
  interpretSync @(Proxy 1) $
  interpretSync @(Proxy 2) $
  interpretSync @(Proxy 3) $
  interpretEventsChan @Int $
  interpretEventsChan @Text do
    thread1 <- async do
      Events.subscribe @Int @(OutChan Int) do
        Sync.putBlock (Proxy @1)
        Events.consume @Int
    thread2 <- async do
      Events.subscribe @Int @(OutChan Int) do
        Sync.putBlock (Proxy @2)
        Events.consume @Int
    thread3 <- async do
      Events.subscribe @Text @(OutChan Text) do
        Sync.putBlock (Proxy @3)
        Events.consume @Text
    Sync.takeBlock @(Proxy 1)
    Sync.takeBlock @(Proxy 2)
    Sync.takeBlock @(Proxy 3)
    Events.publish @Text @(OutChan Text) "test"
    Events.publish @Int @(OutChan Int) 1
    num1 <- await thread1
    num2 <- await thread2
    text1 <- await thread3
    assertJust @_ @IO 1 num1
    assertJust @_ @IO 1 num2
    assertJust @_ @IO "test" text1
