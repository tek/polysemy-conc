module Main where

import Hedgehog (property, test, withTests)
import Polysemy.Conc.Test.EventsTest (test_events)
import Polysemy.Conc.Test.InterruptTest (test_interrupt)
import Polysemy.Conc.Test.LockTest (test_lock)
import Polysemy.Conc.Test.MaskTest (test_mask)
import Polysemy.Conc.Test.MonitorTest (test_monitorBasic, test_monitorClockSkew)
import Polysemy.Conc.Test.QueueTest (
  test_queueBlockTB,
  test_queueBlockTBM,
  test_queuePeekTBM,
  test_queueTB,
  test_queueTBM,
  test_queueTimeoutTBM,
  )
import Polysemy.Conc.Test.ScopedTest (test_scoped)
import Polysemy.Conc.Test.SyncTest (test_sync, test_syncLock)
import Polysemy.Test (unitTest)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup "main" [
    testGroup "queue" [
      unitTest "TBM success" test_queueTBM,
      unitTest "TBM timeout" test_queueTimeoutTBM,
      unitTest "TBM peek" test_queuePeekTBM,
      unitTest "TBM block" test_queueBlockTBM,
      unitTest "TB success" test_queueTB,
      unitTest "TB block" test_queueBlockTB
    ],
    testGroup "events" [
      testProperty "events" (withTests 100 (property (test test_events)))
    ],
    testGroup "sync" [
      unitTest "sync" test_sync,
      unitTest "lock" test_syncLock
    ],
    test_lock,
    testGroup "interrupt" [
      unitTest "interrupt" test_interrupt
    ],
    testGroup "mask" [
      unitTest "mask" test_mask
    ],
    testGroup "monitor" [
      unitTest "basic" test_monitorBasic,
      unitTest "clock skew" test_monitorClockSkew
    ],
    test_scoped
  ]

main :: IO ()
main =
  defaultMain tests
