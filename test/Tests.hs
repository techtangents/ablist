module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.QuickCheck

import Data.ABList

main :: IO ()
main = defaultMain tests

instance (Arbitrary a, Arbitrary b) => Arbitrary (ABList a b) where
  arbitrary =
    do
      as <- arbitrary
      bs <- arbitrary
      let n = return ABNil
      let z = return $ abZip as bs
      frequency [(2, n), (98, z)]

tests :: [Test]
tests =
  [ g_abToListEither
  , g_abFromListEither
  , g_abHead
  , g_abTail
  , g_aaToList
  , g_aaFromList
  , g_aaMap
  , g_abZip
  , g_abFromPairs
  , g_abToPairs
  , g_abFoldr
  , g_abFoldr'
  , g_abMap
  , g_abMerge
  , g_abInit
  , g_abReverse
  , g_abMapLefts
  , g_abMapRights
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


g_abFromPairs =
  testGroup "abFromPairs"
  [ testProperty "0" p_abFromPairs_0
  , testProperty "1" p_abFromPairs_1
  , testProperty "2" p_abFromPairs_2
  , testProperty "n" p_abFromPairs_n
  ]

p_abFromPairs_0 :: Bool
p_abFromPairs_0 = abFromPairs ([] :: [(Double, Int)]) == ABNil

p_abFromPairs_1 :: Int -> Float -> Bool
p_abFromPairs_1 a b = abFromPairs [(a, b)] == a :/ b :/ ABNil

p_abFromPairs_2 :: Int -> Float -> Int -> Float -> Bool
p_abFromPairs_2 a b c d = abFromPairs [(a,b),(c,d)] == a :/ b :/ c :/ d :/ ABNil

p_abFromPairs_n :: [Int] -> Bool
p_abFromPairs_n as =
  let bs = fmap show as
  in  abFromPairs (as `zip` bs) == as `abZip` bs


g_abToPairs =
  testGroup "abUnzip"
  [ testProperty "0" p_abToPairs_0
  , testProperty "1" p_abToPairs_1
  , testProperty "2" p_abToPairs_2
  , testProperty "3" p_abToPairs_3
  , testProperty "round trip" p_abToPairs_roundTrip
  ]

p_abToPairs_0 :: Bool
p_abToPairs_0 = abToPairs ABNil == ([] :: [(Int,Float)])

p_abToPairs_1 :: Int -> Bool
p_abToPairs_1 a = abToPairs (a :/ ABNil) == ([] :: [(Int,Float)])

p_abToPairs_2 :: Int -> Char -> Bool
p_abToPairs_2 a b = abToPairs (b :/ a :/ ABNil) == [(b,a)]

p_abToPairs_3 :: Int -> Char -> Int -> Bool
p_abToPairs_3 a b c = abToPairs (a :/ b :/ c :/ ABNil) == [(a,b)]

p_abToPairs_roundTrip :: [Int] -> Bool
p_abToPairs_roundTrip as =
  let bs = fmap show as
      pairs = as `zip` bs
  in  (abToPairs . abFromPairs) pairs == pairs


g_abFoldr =
  testGroup "abFoldr"
  [ testProperty "0" p_abFoldr_0
  , testProperty "1" p_abFoldr_1
  , testProperty "2" p_abFoldr_2
  , testProperty "list" p_abFoldr_list
  ]

p_abFoldr_0 :: Bool
p_abFoldr_0 = abFoldr undefined "q" (ABNil :: ABList Int String) == "q"

p_abFoldr_1 :: Int -> Bool
p_abFoldr_1 a = abFoldr (\(Left z) t -> show z ++ t) "q" (a :/ ABNil) == show a ++ "q"

p_abFoldr_2 :: Int -> Char -> Bool
p_abFoldr_2 a b = abFoldr (\e t -> (either show show e) ++ t) "q" (a :/ b :/ ABNil) == show a ++ show b ++ "q"

p_abFoldr_list :: [Int] -> Bool
p_abFoldr_list as = abFoldr (\e t -> (either show show e) ++ t) "q" (aaFromList as) == foldr (\e t -> show e ++ t) "q" as


g_abFoldr' =
  testGroup "abFoldr"
  [ testProperty "0" p_abFoldr_0'
  , testProperty "1" p_abFoldr_1'
  , testProperty "2" p_abFoldr_2'
  , testProperty "list" p_abFoldr_list'
  ]

p_abFoldr_0' :: Bool
p_abFoldr_0' = abFoldr' undefined undefined "q" (ABNil :: ABList Int String) == "q"

p_abFoldr_1' :: Int -> Bool
p_abFoldr_1' a = abFoldr' (\z t -> show z ++ t) undefined "q" (a :/ ABNil) == show a ++ "q"

p_abFoldr_2' :: Int -> Char -> Bool
p_abFoldr_2' a b = abFoldr' (\x t -> show x ++ t) (\x t -> show x ++ t) "q" (a :/ b :/ ABNil) == show a ++ show b ++ "q"

p_abFoldr_list' :: [Int] -> Bool
p_abFoldr_list' as = abFoldr' (\x t -> show x ++ t) (\x t -> show x ++ t) "q" (aaFromList as) == foldr (\e t -> show e ++ t) "q" as


g_abMap =
  testGroup "abMap"
  [ testProperty "0" p_abMap_0
  , testProperty "1" p_abMap_1
  , testProperty "2" p_abMap_2
  , testProperty "3" p_abMap_3
  , testProperty "4" p_abMap_4
  , testProperty "id" p_abMap_id
  ]

p_abMap_0 :: Bool
p_abMap_0 = abMap undefined undefined ABNil == (ABNil :: ABList Int Char)

p_abMap_1 :: Int -> Bool
p_abMap_1 a = abMap show undefined (a :/ ABNil) == ((show a) :/ ABNil :: ABList String Bool)

p_abMap_2 :: Int -> Char -> Bool
p_abMap_2 i c = abMap (+1) show (i :/ c :/ ABNil) == (i + 1 :/ show c :/ ABNil)

p_abMap_3 :: Int -> Char -> Int -> Bool
p_abMap_3 i c j = abMap (+1) show (i :/ c :/ j :/ ABNil) == (i + 1 :/ show c :/ j + 1 :/ ABNil)

p_abMap_4 :: Int -> Char -> Int -> Char -> Bool
p_abMap_4 i c j d = abMap (+1) show (i :/ c :/ j :/ d :/ ABNil) == (i + 1 :/ show c :/ j + 1 :/ show d :/ ABNil)

p_abMap_id :: ABList String Float -> Bool
p_abMap_id x = abMap id id x == x

g_abMerge =
  testGroup "abMerge"
  [ testProperty "0" p_abMerge_0
  , testProperty "1" p_abMerge_1
  , testProperty "2" p_abMerge_2
  , testProperty "3" p_abMerge_3
  , testProperty "4" p_abMerge_4
  ]

p_abMerge_0 :: Bool
p_abMerge_0 = abMerge undefined undefined (ABNil :: ABList Int Char) == ([] :: [Char])

p_abMerge_1 :: Int -> Bool
p_abMerge_1 a = abMerge show undefined (abSingle a :: ABList Int Char) == [show a]

p_abMerge_2 :: Int -> String -> Bool
p_abMerge_2 a b = abMerge show id (a :/ b :/ ABNil) == [show a, b]

p_abMerge_3 :: Int -> String -> Int -> Bool
p_abMerge_3 a b c = abMerge show id (a :/ b :/ c :/ ABNil) == [show a, b, show c]

p_abMerge_4 :: Int -> String -> Int -> String -> Bool
p_abMerge_4 a b c d = abMerge show id (a :/ b :/ c :/ d :/ ABNil) == [show a, b, show c, d]


g_abInit =
  testGroup "abInit"
  [ testProperty "0" p_abInit_0
  , testProperty "1" p_abInit_1
  , testProperty "2" p_abInit_2
  , testProperty "3" p_abInit_3
  , testProperty "4" p_abInit_4
  ]

p_abInit_0 :: Bool
p_abInit_0 = abInit (ABNil :: ABList Int Char) == Nothing

p_abInit_1 :: Int -> Bool
p_abInit_1 a = abInit (a :/ ABNil :: ABList Int Char) == Just ABNil

p_abInit_2 :: Int -> Char -> Bool
p_abInit_2 a b = abInit (a :/ b :/ ABNil) == (Just $ abSingle a)

p_abInit_3 :: Float -> (Maybe Int) -> Float -> Bool
p_abInit_3 a b c = abInit (a :/ b :/ c :/ ABNil) == (Just $ a :/ b :/ ABNil)

p_abInit_4 :: (Either Float Int) -> String -> (Either Float Int) -> String -> Bool
p_abInit_4 a b c d = abInit (a :/ b :/ c :/ d :/ ABNil) == (Just $ a :/ b :/ c :/ ABNil)


g_abReverse =
  testGroup "abReverse"
  [ testProperty "0" p_abReverse_0
  , testProperty "1" p_abReverse_1
  , testProperty "2" p_abReverse_2
  , testProperty "3" p_abReverse_3
  , testProperty "4" p_abReverse_4
  , testProperty "n" p_abReverse_n
  , testProperty "m" p_abReverse_m
  ]

p_abReverse_0 :: Bool
p_abReverse_0 = abReverse (ABNil :: ABList Int Char) == Right ABNil

p_abReverse_1 :: Int -> Bool
p_abReverse_1 a = abReverse (a :/ ABNil :: ABList Int Char) == (Left $ a :/ ABNil)

p_abReverse_2 :: Int -> Char -> Bool
p_abReverse_2 a c = abReverse (a :/ c :/ ABNil) == (Right $ c :/ a :/ ABNil)

p_abReverse_3 :: Int -> Char -> Int -> Bool
p_abReverse_3 a b c = abReverse (a :/ b :/ c :/ ABNil) == (Left $ c :/ b :/ a :/ ABNil)

p_abReverse_4 :: Int -> Char -> Int -> Char -> Bool
p_abReverse_4 a b c d = abReverse (a :/ b :/ c :/ d :/ ABNil) == (Right $ d :/ c :/ b :/ a :/ ABNil)

p_abReverse_n :: [Int] -> Bool
p_abReverse_n is =
  let cs = fmap show is
      z = abZip is cs
      z' = abZip (reverse cs) (reverse is)
  in  abReverse z == Right z'

p_abReverse_m :: [Int] -> String -> Bool
p_abReverse_m is c =
  let cs = c : (fmap show is)
      z = abZip cs is
      z' = abZip (reverse cs) (reverse is)
  in  abReverse z == Left z'


g_abMapLefts =
  testGroup "abMapLefts"
  [ testProperty "0" p_abMapLefts_0
  , testProperty "1" p_abMapLefts_1
  , testProperty "2" p_abMapLefts_2
  , testProperty "3" p_abMapLefts_3
  , testProperty "id" p_abMapLefts_id
  ]

p_abMapLefts_0 :: Bool
p_abMapLefts_0 = abMapLefts (undefined :: Int -> String) (ABNil :: ABList Int Char) == (ABNil :: ABList String Char)

p_abMapLefts_1 :: Int -> Bool
p_abMapLefts_1 a = abMapLefts show (a :/ ABNil :: ABList Int Char) == (show a) :/ ABNil

p_abMapLefts_2 :: Int -> Char -> Bool
p_abMapLefts_2 a b = abMapLefts show (a :/ b :/ ABNil) == (show a) :/ b :/ ABNil

p_abMapLefts_3 :: Int -> Char -> Int -> Bool
p_abMapLefts_3 a b c = abMapLefts show (a :/ b :/ c :/ ABNil) == (show a) :/ b :/ (show c) :/ ABNil

p_abMapLefts_id :: ABList Int Char -> Bool
p_abMapLefts_id x = abMapLefts id x == x


g_abMapRights =
  testGroup "abMapRights"
  [ testProperty "0" p_abMapRights_0
  , testProperty "1" p_abMapRights_1
  , testProperty "2" p_abMapRights_2
  , testProperty "3" p_abMapRights_3
  , testProperty "id" p_abMapRights_id
  ]

p_abMapRights_0 :: Bool
p_abMapRights_0 = abMapRights (undefined :: Int -> String) (ABNil :: ABList Char Int) == (ABNil :: ABList Char String)

p_abMapRights_1 :: Char -> Bool
p_abMapRights_1 a = abMapRights (undefined :: Int -> String) (a :/ ABNil :: ABList Char Int) == a :/ ABNil

p_abMapRights_2 :: Int -> Char -> Bool
p_abMapRights_2 a b = abMapRights show (a :/ b :/ ABNil) == a :/ (show b) :/ ABNil

p_abMapRights_3 :: Int -> Char -> Int -> Bool
p_abMapRights_3 a b c = abMapRights show (a :/ b :/ c :/ ABNil) == a :/ (show b) :/ c :/ ABNil

p_abMapRights_4 :: Int -> Char -> Int -> Char -> Bool
p_abMapRights_4 a b c d = abMapRights show (a :/ b :/ c :/ d :/ ABNil) == a :/ (show b) :/ c :/ (show d) :/ ABNil

p_abMapRights_id :: ABList Int Char -> Bool
p_abMapRights_id x = abMapRights id x == x
