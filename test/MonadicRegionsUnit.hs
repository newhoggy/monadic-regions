module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit                           hiding (Test, Testable)

main :: IO ()
main = defaultMain
       [
         testCase "Failing test" failing
       ]

failing :: Assertion
failing = False @? "Failing test reason"

