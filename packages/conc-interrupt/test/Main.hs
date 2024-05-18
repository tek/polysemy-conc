module Main where

import Polysemy.Conc.Test.InterruptTest (test_interrupt)
import Polysemy.Test (unitTest)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "main" [
    testGroup "interrupt" [
      unitTest "interrupt" test_interrupt
    ]
  ]

main :: IO ()
main =
  defaultMain tests
