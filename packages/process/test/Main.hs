module Main where

import Polysemy.Process.Test.InterruptTest (test_interrupt)
import Polysemy.Process.Test.ProcessTest (test_processAll)
import Polysemy.Test (unitTestTimes)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "main" [
    test_processAll,
    testGroup "interrupt" [
      unitTestTimes 100 "interrupt" test_interrupt
    ]
  ]

main :: IO ()
main =
  defaultMain tests
