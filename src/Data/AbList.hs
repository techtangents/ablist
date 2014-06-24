module Data.AbList where

import Prelude

infixr 5 :/

-- | An alternating list of two types. 
-- e.g.
--
-- > 3 :/ "a" :/ 7 :/ "b" :/ AbNil
--
data AbList a b            
  = AbNil                  -- ^ An empty AbList
  | a :/ AbList b a        -- ^ Cons of a head to an AbList
  deriving (Eq, Ord, Show)

abSingle :: a -> AbList a b
abSingle a = a :/ AbNil

abShallowFold :: t -> (a -> AbList b a -> t) -> (AbList a b) -> t
abShallowFold t _ AbNil = t
abShallowFold _ t (a :/ ba) = t a ba

abToListEither :: AbList a b -> [Either a b]
abToListEither AbNil = []
abToListEither (a :/ AbNil) = [Left a]
abToListEither (a :/ b :/ abz) = (Left a) : (Right b) : abToListEither abz

abFromListEither :: [Either a b] -> Maybe (AbList a b)
abFromListEither [] = Just AbNil
abFromListEither (Left a : []) = Just $ a :/ AbNil
abFromListEither (Left a : Right b : xs) = fmap (\t -> a :/ b :/ t) (abFromListEither xs)
abFromListEither _ = Nothing

abHead :: AbList a b -> Maybe a
abHead = abShallowFold Nothing $ const . Just

abTail :: AbList a b -> Maybe (AbList b a)
abTail = abShallowFold Nothing $ const Just

abInit :: AbList a b -> Maybe (AbList a b)
abInit AbNil = Nothing
abInit (a :/ as) = Just $ q a as
  where
    q :: a -> AbList b a -> AbList a b
    q _ AbNil = AbNil
    q x (y :/ zs) = x :/ q y zs

aaToList :: AbList a a -> [a]
aaToList AbNil = []
aaToList (a :/ as) = a : (aaToList as)

aaMap :: (a -> b) -> AbList a a -> AbList b b
aaMap _ AbNil = AbNil
aaMap f (a :/ as) = f a :/ (aaMap f as)

aaFromList :: [a] -> AbList a a
aaFromList = foldr (:/) AbNil

abFoldr :: (Either a b -> t -> t) -> t -> (AbList a b) -> t
abFoldr f = abFoldr' (f . Left) (f . Right)

abFoldr' :: (a -> t -> t) -> (b -> t -> t) -> t -> (AbList a b) -> t
abFoldr' _ _ t AbNil = t
abFoldr' f _ t (a :/ AbNil) = f a t
abFoldr' f g t (a :/ b :/ cs) = f a (g b (abFoldr' f g t cs))

abFoldl :: (t -> Either a b -> t) -> t -> (AbList a b) -> t
abFoldl f = abFoldl' ((. Left) . f) ((. Right) . f)

abFoldl' :: (t -> a -> t) -> (t -> b -> t) -> t -> (AbList a b) -> t
abFoldl' _ _ t AbNil = t
abFoldl' f _ t (a :/ AbNil) = f t a
abFoldl' f g t (a :/ b :/ cs) = abFoldl' f g (f (g t b) a) cs

abZip :: [a] -> [b] -> AbList a b
abZip [] _ = AbNil
abZip (a:_) [] = a :/ AbNil
abZip (a:as) (b:bs) = a :/ b :/ (abZip as bs)

abFromPairs :: [(a, b)] -> AbList a b
abFromPairs = foldr (\(a, b) cs -> a :/ b :/ cs) AbNil

abToPairs :: AbList a b -> [(a, b)]
abToPairs AbNil = []
abToPairs (_ :/ AbNil) = []
abToPairs (a :/ b :/ cs) = (a,b) : abToPairs cs

abMap :: (a -> a') -> (b -> b') -> AbList a b -> AbList a' b'
abMap _ _ AbNil = AbNil
abMap fa _ (a :/ AbNil) = fa a :/ AbNil
abMap fa fb (a :/ b :/ cs) = fa a :/ fb b :/ (abMap fa fb cs)

abMerge :: (a -> t) -> (b -> t) -> AbList a b -> [t]
abMerge f g = aaToList . (abMap f g)

-- thanks to Travis Cardwell ("tcard")
abReverse :: AbList a b -> Either (AbList a b) (AbList b a)
abReverse = goAB AbNil
 where
   goAB :: AbList b a -> AbList a b -> Either (AbList a b) (AbList b a)
   goAB acc AbNil = Right acc
   goAB acc (x :/ xs) = goBA (x :/ acc) xs
   goBA :: AbList a b -> AbList b a -> Either (AbList a b) (AbList b a)
   goBA acc AbNil = Left acc
   goBA acc (x :/ xs) = goAB (x :/ acc) xs

abMapLefts :: (a -> a') -> AbList a b -> AbList a' b
abMapLefts = flip abMap id

abMapRights :: (b -> b') -> AbList a b -> AbList a b'
abMapRights = abMap id

-- right-biased functor
instance Functor (AbList a) where
  fmap = abMapRights

