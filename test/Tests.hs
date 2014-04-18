module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.QuickCheck

import qualified AbListTests (tests)
import qualified AbneListTests (tests)

main :: IO ()
main = defaultMain [tests]

tests =
  testGroup "the"
  [ AbListTests.tests
  , AbneListTests.tests
  ]
