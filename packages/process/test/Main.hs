module Main where

import Polysemy.Process.Test.ProcessTest (test_process)
import Polysemy.Test (unitTest)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "main" [
    testGroup "process" [
      unitTest "process" test_process
    ]
  ]

main :: IO ()
main =
  defaultMain tests
