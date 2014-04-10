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
    g_abTail,
    g_aaToList,
    g_aaFromList,
    g_aaMap,
    g_abZip,
    g_abZipPairs
  ]

g_abToListEither =
  testGroup "abToListEither"
  [ testProperty "empty" p_abToListEither_Empty
  , testProperty "1" p_abToListEither_1
  , testProperty "2" p_abToListEither_2
  , testProperty "3" p_abToListEither_3
  , testProperty "4" p_abToListEither_4
  ]

p_abToListEither_Empty :: Bool
p_abToListEither_Empty = (abToListEither ABNil) == ([] :: [Either String Int])

p_abToListEither_1 :: Int -> Bool
p_abToListEither_1 a = (abToListEither $ a :/ ABNil) == ([Left a] :: [Either Int String])

p_abToListEither_2 :: Int -> String -> Bool
p_abToListEither_2 a b = (abToListEither $ a :/ b :/ ABNil) == ([Left a, Right b])

p_abToListEither_3 :: Int -> String -> Int -> Bool
p_abToListEither_3 a b c = (abToListEither $ a :/ b :/ c :/ ABNil) == ([Left a, Right b, Left c])

p_abToListEither_4 :: Int -> String -> Int -> String -> Bool
p_abToListEither_4 a b c d = (abToListEither $ a :/ b :/ c :/ d :/ ABNil) == ([Left a, Right b, Left c, Right d])


g_abFromListEither =
  testGroup "abFromListEither"
  [ testProperty "empty" p_abFromListEither_empty
  , testProperty "g" p_abFromListEither_g
  , testProperty "gg" p_abFromListEither_gg
  , testProperty "ggg" p_abFromListEither_ggg
  , testProperty "gggg" p_abFromListEither_gggg
  , testProperty "ggggg" p_abFromListEither_gggg
  , testProperty "b" p_abFromListEither_b
  , testProperty "gb" p_abFromListEither_gb
  , testProperty "ggb" p_abFromListEither_gb
  ]

p_abFromListEither_empty :: Bool
p_abFromListEither_empty = abFromListEither ([] :: [Either Int String]) == Just ABNil

p_abFromListEither_g :: Int -> Bool
p_abFromListEither_g a = abFromListEither ([Left a] :: [Either Int String]) == Just (a :/ ABNil)

p_abFromListEither_gg :: Int -> Float -> Bool
p_abFromListEither_gg a b = abFromListEither ([Left a, Right b]) == Just (a :/ b :/ ABNil)

p_abFromListEither_ggg :: Int -> Float -> Int -> Bool
p_abFromListEither_ggg a b c = abFromListEither [Left a, Right b, Left c] == Just (a :/ b :/ c :/ ABNil)

p_abFromListEither_gggg :: Int -> Float -> Int -> Float -> Bool
p_abFromListEither_gggg a b c d = abFromListEither [Left a, Right b, Left c, Right d] == Just (a :/ b :/ c :/ d :/ ABNil)

p_abFromListEither_ggggg :: Int -> Float -> Int -> Float -> Int -> Bool
p_abFromListEither_ggggg a b c d e = abFromListEither [Left a, Right b, Left c, Right d] == Just (a :/ b :/ c :/ d :/ e :/ ABNil)

p_abFromListEither_b :: String -> Bool
p_abFromListEither_b a = abFromListEither ((Right a : undefined) :: [Either Int String]) == Nothing

p_abFromListEither_gb :: Bool
p_abFromListEither_gb = abFromListEither ((Left undefined : Left undefined : undefined) :: [Either Int String]) == Nothing

p_abFromListEither_ggb :: String -> Char -> Bool
p_abFromListEither_ggb a b = abFromListEither (Left a : Right b : Left undefined : undefined) == Nothing


g_abHead =
  testGroup "abHead"
  [ testProperty "empty" p_abHead_empty
  , testProperty "n" p_abHead_n
  ]

p_abHead_empty :: Bool
p_abHead_empty = abHead (ABNil :: ABList Int Float) == Nothing

p_abHead_n :: Int -> Bool
p_abHead_n a = abHead (a :/ undefined :: ABList Int Float) == Just a


g_abTail =
  testGroup "abTail"
  [ testProperty "empty" p_abTail_empty
  , testProperty "1" p_abTail_1
  , testProperty "2" p_abTail_2
  ]

p_abTail_empty :: Bool
p_abTail_empty = abTail (ABNil :: ABList Int Float) == Nothing

p_abTail_1 :: Int -> Bool
p_abTail_1 a = abTail (a :/ ABNil :: ABList Int String) == Just ABNil

p_abTail_2 :: Int -> String -> Bool
p_abTail_2 a b = abTail (a :/ b :/ ABNil :: ABList Int String) == (Just $ b :/ ABNil)


g_aaToList =
  testGroup "aaToList"
  [ testProperty "empty" p_aaToList_empty
  , testProperty "1" p_aaToList_1
  , testProperty "2" p_aaToList_2
  , testProperty "3" p_aaToList_3
  ]

p_aaToList_empty :: Bool
p_aaToList_empty = aaToList ABNil == ([] :: [Int])

p_aaToList_1 :: Int -> Bool
p_aaToList_1 a = aaToList (a :/ ABNil) == [a]

p_aaToList_2 :: Int -> Int -> Bool
p_aaToList_2 a b = aaToList (a :/ b :/ ABNil) == [a, b]

p_aaToList_3 :: Int -> Int -> Int -> Bool
p_aaToList_3 a b c = aaToList (a :/ b :/ c :/ ABNil) == [a, b, c]


g_aaFromList =
  testGroup "aaFromList"
  [ testProperty "empty" p_aaFromList_empty
  , testProperty "1" p_aaFromList_1
  , testProperty "2" p_aaFromList_2
  , testProperty "3" p_aaFromList_3
  , testProperty "roundTrip" p_aaToList_roundTrip
  ]

p_aaFromList_empty :: Bool
p_aaFromList_empty = aaFromList ([] :: [Int]) == ABNil

p_aaFromList_1 :: Int -> Bool
p_aaFromList_1 a = aaFromList [a] == a :/ ABNil

p_aaFromList_2 :: Int -> Int -> Bool
p_aaFromList_2 a b = aaFromList [a, b] == a :/ b :/ ABNil

p_aaFromList_3 :: Int -> Int -> Int -> Bool
p_aaFromList_3 a b c = aaFromList [a, b, c] == a :/ b :/ c :/ ABNil

p_aaToList_roundTrip :: [Int] -> Bool
p_aaToList_roundTrip as = (aaToList . aaFromList) as == as


g_aaMap =
  testGroup "aaMap"
  [ testProperty "a" p_aaMap
  ]

p_aaMap :: [Int] -> Bool
p_aaMap as =
  let f a = a + 2
  in  aaToList (aaMap f (aaFromList as)) == fmap f as


g_abZip =
  testGroup "abZip"
  [ testProperty "0 0" p_abZip_0_0
  , testProperty "1 0" p_abZip_1_0
  , testProperty "1 1" p_abZip_1_1
  , testProperty "2 1" p_abZip_2_1
  , testProperty "2 2" p_abZip_2_2
  ]

p_abZip_0_0 :: Bool
p_abZip_0_0 = abZip ([] :: [String]) (undefined :: [Float]) == ABNil

p_abZip_1_0 :: Int -> Bool
p_abZip_1_0 a = abZip (a : undefined) ([] :: [String]) == a :/ ABNil

p_abZip_1_1 :: Int -> Float -> Bool
p_abZip_1_1 a b = abZip [a] (b : undefined) == a :/ b :/ ABNil

p_abZip_2_1 :: Int -> Float -> Int -> Bool
p_abZip_2_1 a b c = abZip (a : c : undefined) [b] == a :/ b :/ c :/ ABNil

p_abZip_2_2 :: Int -> Float -> Int -> Float -> Bool
p_abZip_2_2 a b c d = abZip [a, c] (b : d : undefined) == a :/ b :/ c :/ d :/ ABNil


g_abZipPairs =
  testGroup "abZipPairs"
  [ testProperty "0" p_abZipPairs_0
  , testProperty "1" p_abZipPairs_1
  , testProperty "2" p_abZipPairs_2
  , testProperty "3" p_abZipPairs_3
  ]

p_abZipPairs_0 :: Bool
p_abZipPairs_0 = abZipPairs ([] :: [(Double, Int)]) == ABNil

p_abZipPairs_1 :: Int -> Float -> Bool
p_abZipPairs_1 a b = abZipPairs [(a, b)] == a :/ b :/ ABNil

p_abZipPairs_2 :: Int -> Float -> Int -> Float -> Bool
p_abZipPairs_2 a b c d = abZipPairs [(a,b),(c,d)] == a :/ b :/ c :/ d :/ ABNil

p_abZipPairs_3 :: Int -> Float -> Int -> Float -> Int -> Float -> Bool
p_abZipPairs_3 a b c d e f = abZipPairs [(a,b),(c,d),(e,f)] == a :/ b :/ c :/ d :/ e :/ f :/ ABNil

