{-# LANGUAGE Rank2Types, ConstraintKinds #-}

module Data.ABList (
  ABList(..),
  (./),
  abSingle,
  abFoldr,
  abFoldr',
  abToListEither,
  abFromListEither,
  aaToList,
  aaFromList,
  abHead,
  abTail,
  abInit,
  aaMap,
  abMap,
  abZip,
  abFromPairs,
  abToPairs,
  abMerge,
  abReverse,
  abMapLefts,
  abMapRights

) where

import Prelude

infixr 5 :/
data ABList a b = ABNil | a :/ ABList b a
  deriving (Eq, Ord, Show)

infixr 5 ./
(./) :: a -> b -> ABList a b
(./) a b = a :/ b :/ ABNil

abSingle :: a -> ABList a b
abSingle a = a :/ ABNil

shallowFold :: t -> (a -> ABList b a -> t) -> (ABList a b) -> t
shallowFold t _ ABNil = t
shallowFold _ t (a :/ ba) = t a ba

abToListEither :: ABList a b -> [Either a b]
abToListEither ABNil = []
abToListEither (a :/ ABNil) = [Left a]
abToListEither (a :/ b :/ abz) = (Left a) : (Right b) : abToListEither abz

-- either backwards
rehtie :: Either a b -> Either b a
rehtie = either Right Left

rehties :: [Either a b] -> [Either b a]
rehties = fmap rehtie

abFromListEither :: [Either a b] -> Maybe (ABList a b)
abFromListEither [] = Just ABNil
abFromListEither (Left a : as) = fmap (a :/) (abFromListEither . rehties $ as)
abFromListEither (Right _ : _) = Nothing

abHead :: ABList a b -> Maybe a
abHead = shallowFold Nothing $ const . Just

abTail :: ABList a b -> Maybe (ABList b a)
abTail = shallowFold Nothing $ const Just

abInit' :: a -> ABList b a -> ABList a b
abInit' _ ABNil = ABNil
abInit' x (y :/ zs) = x :/ abInit' y zs

abInit :: ABList a b -> Maybe (ABList a b)
abInit ABNil = Nothing
abInit (a :/ as) = Just $ abInit' a as

aaToList :: ABList a a -> [a]
aaToList ABNil = []
aaToList (a :/ as) = a : (aaToList as)

aaMap :: (a -> b) -> ABList a a -> ABList b b
aaMap _ ABNil = ABNil
aaMap f (a :/ as) = f a :/ (aaMap f as)

aaFromList :: [a] -> ABList a a
aaFromList = foldr (:/) ABNil

abFoldr :: (Either a b -> t -> t) -> t -> (ABList a b) -> t
abFoldr f = abFoldr' (f . Left) (f . Right)

abFoldr' :: (a -> t -> t) -> (b -> t -> t) -> t -> (ABList a b) -> t
abFoldr' _ _ t ABNil = t
abFoldr' f _ t (a :/ ABNil) = f a t
abFoldr' f g t (a :/ b :/ cs) = f a (g b (abFoldr' f g t cs))

abFoldl :: (t -> Either a b -> t) -> t -> (ABList a b) -> t
abFoldl f = abFoldl' ((. Left) . f) ((. Right) . f)

abFoldl' :: (t -> a -> t) -> (t -> b -> t) -> t -> (ABList a b) -> t
abFoldl' _ _ t ABNil = t
abFoldl' f _ t (a :/ ABNil) = f t a
abFoldl' f g t (a :/ b :/ cs) = abFoldl' f g (f (g t b) a) cs

abZip :: [a] -> [b] -> ABList a b
abZip [] _ = ABNil
abZip (a:_) [] = a :/ ABNil
abZip (a:as) (b:bs) = a :/ b :/ (abZip as bs)

abFromPairs :: [(a, b)] -> ABList a b
abFromPairs = foldr (\(a, b) cs -> a :/ b :/ cs) ABNil

abToPairs :: ABList a b -> [(a, b)]
abToPairs ABNil = []
abToPairs (_ :/ ABNil) = []
abToPairs (a :/ b :/ cs) = (a,b) : abToPairs cs

abMap :: (a -> a') -> (b -> b') -> ABList a b -> ABList a' b'
abMap _ _ ABNil = ABNil
abMap fa _ (a :/ ABNil) = fa a :/ ABNil
abMap fa fb (a :/ b :/ cs) = fa a :/ fb b :/ (abMap fa fb cs)

abMerge :: (a -> t) -> (b -> t) -> ABList a b -> [t]
abMerge f g = aaToList . (abMap f g)

-- thanks to Travis Cardwell ("tcard")
abReverse :: ABList a b -> Either (ABList a b) (ABList b a)
abReverse = goAB ABNil
 where
   goAB :: ABList b a -> ABList a b -> Either (ABList a b) (ABList b a)
   goAB acc ABNil = Right acc
   goAB acc (x :/ xs) = goBA (x :/ acc) xs
   goBA :: ABList a b -> ABList b a -> Either (ABList a b) (ABList b a)
   goBA acc ABNil = Left acc
   goBA acc (x :/ xs) = goAB (x :/ acc) xs

abMapLefts :: (a -> a') -> ABList a b -> ABList a' b
abMapLefts = flip abMap id

abMapRights :: (b -> b') -> ABList a b -> ABList a b'
abMapRights = abMap id

-- right-biased functor
instance Functor (ABList a) where
  fmap = abMapRights

