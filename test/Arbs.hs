module Arbs where

import Test.QuickCheck
import Data.ABList

instance (Arbitrary a, Arbitrary b) => Arbitrary (ABList a b) where
  arbitrary =
    do
      as <- arbitrary
      bs <- arbitrary
      let n = return ABNil
      let z = return $ abZip as bs
      frequency [(2, n), (98, z)]