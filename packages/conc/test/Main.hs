module Main where

import Polysemy.Conc.Test.InterruptTest (test_interrupt)
import Polysemy.Conc.Test.QueueTest (
  test_queueBlockTB,
  test_queueBlockTBM,
  test_queuePeekTBM,
  test_queueTB,
  test_queueTBM,
  test_queueTimeoutTBM,
  )
import Polysemy.Conc.Test.SyncTest (test_sync)
import Polysemy.Test (unitTest)
import Test.Tasty (TestTree, defaultMain, testGroup)

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
    testGroup "sync" [
      unitTest "sync" test_sync
      ],
    testGroup "interrupt" [
      unitTest "interrupt" test_interrupt
      ]
    ]

main :: IO ()
main =
  defaultMain tests
