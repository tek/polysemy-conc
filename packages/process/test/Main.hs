module Main where

import Polysemy.Process.Test.ProcessTest (test_processAll)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "main" [
    test_processAll
  ]

main :: IO ()
main =
  defaultMain tests
