module Arbs where

import Test.QuickCheck
import Data.AbList

instance (Arbitrary a, Arbitrary b) => Arbitrary (AbList a b) where
  arbitrary =
    do
      as <- arbitrary
      bs <- arbitrary
      let n = return AbNil
      let z = return $ abZip as bs
      frequency [(2, n), (98, z)]