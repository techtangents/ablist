module Data.AbneList where

import Data.AbList

infixr 5 ://
data AbneList a b = a :// AbList b a
  deriving (Eq, Ord, Show)

abneHead :: AbneList a b -> a
abneHead (a :// _) = a

abneTail :: AbneList a b -> AbList b a
abneTail (_ :// as) = as

--test
abneShallowFold :: (a -> AbList b a -> t) -> AbneList a b -> t
abneShallowFold f (a :// as) = f a as

toAbList :: AbneList a b -> AbList a b
toAbList = abneShallowFold (:/)

fromAbList :: AbList a b -> Maybe (AbneList a b)
fromAbList = abShallowFold Nothing $ (Just .) . (://)

-- TODO: convert to a NonEmpty (Either a b) <-- which NonEmpty lib, though?
abneToListEither :: AbneList a b -> [Either a b]
abneToListEither = abToListEither . toAbList

abneFromListEither :: [Either a b] -> Maybe (AbneList a b)
abneFromListEither =
  (fromAbList =<<) . abFromListEither

abneInit :: AbneList a b -> AbList a b
abneInit = abneShallowFold q
  where
    q :: a -> AbList b a -> AbList a b
    q _ AbNil = AbNil
    q x (y :/ zs) = x :/ q y zs

aaneToList :: AbneList a a -> [a]
aaneToList = aaToList . toAbList

aaneMap :: (a -> b) -> AbneList a a -> AbneList b b
aaneMap f (a :// as) = f a :// aaMap f as

aaneFromList :: [a] -> Maybe (AbneList a a)
aaneFromList = fromAbList . aaFromList

abneFoldr :: (Either a b -> t -> t) -> t -> (AbneList a b) -> t
abneFoldr f t = abFoldr f t . toAbList

abneFoldr' :: (a -> t -> t) -> (b -> t -> t) -> t -> (AbneList a b) -> t
abneFoldr' f g t = abFoldr' f g t . toAbList

abneFoldl :: (t -> Either a b -> t) -> t -> (AbneList a b) -> t
abneFoldl f t = abFoldl f t . toAbList

abneFoldl' :: (t -> a -> t) -> (t -> b -> t) -> t -> (AbneList a b) -> t
abneFoldl' f g t = abFoldl' f g t . toAbList

abneZip :: [a] -> [b] -> Maybe (AbneList a b)
abneZip = (fromAbList .) . abZip

abneFromPairs :: [(a, b)] -> Maybe (AbneList a b)
abneFromPairs = fromAbList . abFromPairs

-- reduce
-- reverse
-- map
-- functor