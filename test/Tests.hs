module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.QuickCheck

import qualified ABListTests (tests)
import qualified ABNEListTests (tests)

main :: IO ()
main = defaultMain [tests]

tests =
  testGroup "the"
  [ ABListTests.tests
  , ABNEListTests.tests
  ]
