module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.QuickCheck

import Data.ABList

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [
    g_abToListEither,
    g_abFromListEither,
    g_abHead,
    g_abTail
  ]

g_abToListEither =
  testGroup "abToListEither"
  [ testProperty "empty" prop_abToListEither_Empty
  , testProperty "1" prop_abToListEither_1
  , testProperty "2" prop_abToListEither_2
  , testProperty "3" prop_abToListEither_3
  , testProperty "4" prop_abToListEither_4
  ]

prop_abToListEither_Empty :: Bool
prop_abToListEither_Empty = (abToListEither ABNil) == ([] :: [Either String Int])

prop_abToListEither_1 :: Int -> Bool
prop_abToListEither_1 a = (abToListEither $ a :/ ABNil) == ([Left a] :: [Either Int String])

prop_abToListEither_2 :: Int -> String -> Bool
prop_abToListEither_2 a b = (abToListEither $ a :/ b :/ ABNil) == ([Left a, Right b])

prop_abToListEither_3 :: Int -> String -> Int -> Bool
prop_abToListEither_3 a b c = (abToListEither $ a :/ b :/ c :/ ABNil) == ([Left a, Right b, Left c])

prop_abToListEither_4 :: Int -> String -> Int -> String -> Bool
prop_abToListEither_4 a b c d = (abToListEither $ a :/ b :/ c :/ d :/ ABNil) == ([Left a, Right b, Left c, Right d])


g_abFromListEither =
  testGroup "abFromListEither"
  [ testProperty "empty" prop_abFromListEither_empty
  , testProperty "g" prop_abFromListEither_g
  , testProperty "gg" prop_abFromListEither_gg
  , testProperty "ggg" prop_abFromListEither_ggg
  , testProperty "gggg" prop_abFromListEither_gggg
  , testProperty "ggggg" prop_abFromListEither_gggg
  , testProperty "b" prop_abFromListEither_b
  , testProperty "gb" prop_abFromListEither_gb
  , testProperty "ggb" prop_abFromListEither_gb
  ]

prop_abFromListEither_empty :: Bool
prop_abFromListEither_empty = abFromListEither ([] :: [Either Int String]) == Just ABNil

prop_abFromListEither_g :: Int -> Bool
prop_abFromListEither_g a = abFromListEither ([Left a] :: [Either Int String]) == Just (a :/ ABNil)

prop_abFromListEither_gg :: Int -> Float -> Bool
prop_abFromListEither_gg a b = abFromListEither ([Left a, Right b]) == Just (a :/ b :/ ABNil)

prop_abFromListEither_ggg :: Int -> Float -> Int -> Bool
prop_abFromListEither_ggg a b c = abFromListEither [Left a, Right b, Left c] == Just (a :/ b :/ c :/ ABNil)

prop_abFromListEither_gggg :: Int -> Float -> Int -> Float -> Bool
prop_abFromListEither_gggg a b c d = abFromListEither [Left a, Right b, Left c, Right d] == Just (a :/ b :/ c :/ d :/ ABNil)

prop_abFromListEither_ggggg :: Int -> Float -> Int -> Float -> Int -> Bool
prop_abFromListEither_ggggg a b c d e = abFromListEither [Left a, Right b, Left c, Right d] == Just (a :/ b :/ c :/ d :/ e :/ ABNil)

prop_abFromListEither_b :: String -> Bool
prop_abFromListEither_b a = abFromListEither ((Right a : undefined) :: [Either Int String]) == Nothing

prop_abFromListEither_gb :: Bool
prop_abFromListEither_gb = abFromListEither ((Left undefined : Left undefined : undefined) :: [Either Int String]) == Nothing

prop_abFromListEither_ggb :: String -> Char -> Bool
prop_abFromListEither_ggb a b = abFromListEither (Left a : Right b : Left undefined : undefined) == Nothing


g_abHead =
  testGroup "abHead"
  [ testProperty "empty" prop_abHead_empty
  , testProperty "n" prop_abHead_n
  ]

prop_abHead_empty :: Bool
prop_abHead_empty = abHead (ABNil :: ABList Int Float) == Nothing

prop_abHead_n :: Int -> Bool
prop_abHead_n a = abHead (a :/ undefined :: ABList Int Float) == Just a

g_abTail =
  testGroup "abTail"
  [ testProperty "empty" prop_abTail_empty
  , testProperty "1" prop_abTail_1
  , testProperty "2" prop_abTail_2
  ]

prop_abTail_empty :: Bool
prop_abTail_empty = abTail (ABNil :: ABList Int Float) == Nothing

prop_abTail_1 :: Int -> Bool
prop_abTail_1 a = abTail (a :/ ABNil :: ABList Int String) == Just ABNil

prop_abTail_2 :: Int -> String -> Bool
prop_abTail_2 a b = abTail (a :/ b :/ ABNil :: ABList Int String) == (Just $ b :/ ABNil)



