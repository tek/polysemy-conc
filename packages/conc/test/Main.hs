module Main where

import Polysemy.Conc.Test.EventsTest (test_events)
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
import Polysemy.Conc.Test.Run (unitTestTimes)
import Polysemy.Conc.Test.SyncTest (test_sync, test_syncLock)
import Test.Tasty (TestTree, Timeout (NoTimeout), adjustOption, defaultMain, mkTimeout, testGroup)

defaultTimeout :: Timeout -> Timeout
defaultTimeout = \case
  NoTimeout -> mkTimeout 60_000_000
  t -> t

tests :: TestTree
tests =
  adjustOption defaultTimeout $
  testGroup "main" [
    testGroup "queue" [
      unitTestTimes 100 "TBM success" test_queueTBM,
      unitTestTimes 100 "TBM timeout" test_queueTimeoutTBM,
      unitTestTimes 100 "TBM peek" test_queuePeekTBM,
      unitTestTimes 10 "TBM block" test_queueBlockTBM,
      unitTestTimes 100 "TB success" test_queueTB,
      unitTestTimes 10 "TB block" test_queueBlockTB
    ],
    testGroup "events" [
      unitTestTimes 100 "events" test_events
    ],
    testGroup "sync" [
      unitTestTimes 100 "sync" test_sync,
      unitTestTimes 10 "lock" test_syncLock
    ],
    test_lock,
    testGroup "mask" [
      unitTestTimes 10 "mask" test_mask
    ],
    testGroup "monitor" [
      unitTestTimes 100 "basic" test_monitorBasic,
      unitTestTimes 100 "clock skew" test_monitorClockSkew
    ]
  ]

main :: IO ()
main = defaultMain tests
