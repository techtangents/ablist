module AbneListTests where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.QuickCheck
import Data.AbList
import Data.AbneList
import Arbs

tests :: Test
tests =
  testGroup "AbneList"
  [ g_abneHeadTail
  ]

g_abneHeadTail =
  testGroup "abneHead"
  [ testProperty "h" p_abneHead_h
  , testProperty "t" p_abneHead_t
  , testProperty "ht" p_abneHead_ht
  ]

p_abneHead_h :: Int -> AbList Char Int -> Bool
p_abneHead_h a as = (abneHead (a :// as)) == a

p_abneHead_t :: Int -> AbList Char Int -> Bool
p_abneHead_t a as = (abneTail (a :// as)) == as

p_abneHead_ht :: Int -> AbList Char Int -> Bool
p_abneHead_ht a as =
  let o = a :// as
  in  abneHead o :// abneTail o == o

