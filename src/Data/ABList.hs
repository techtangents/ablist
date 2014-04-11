module Data.ABList (
  ABList(..),
  abFoldr,
  abFoldr',
  abToListEither,
  abFromListEither,
  aaToList,
  aaFromList,
  abHead,
  abTail,
  aaMap,
  abMap,
  abZip,
  abFromPairs,
  abToPairs,
  abMerge
) where

infixr 5 :/

data ABList a b = ABNil | a :/ ABList b a
  deriving (Eq, Ord, Show)

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

abInit :: ABList a b -> Maybe (ABList a b)
abInit = undefined

aaToList :: ABList a a -> [a]
aaToList ABNil = []
aaToList (a :/ as) = a : (aaToList as)

aaMap :: (a -> b) -> ABList a a -> ABList b b
aaMap _ ABNil = ABNil
aaMap f (a :/ as) = f a :/ (aaMap f as)

aaFromList :: [a] -> ABList a a
aaFromList = foldr (:/) ABNil

abFoldr :: ((Either a b) -> t -> t) -> t -> (ABList a b) -> t
abFoldr _ t ABNil = t
abFoldr f t (a :/ ABNil) = f (Left a) t
abFoldr f t (a :/ b :/ cs) = f (Left a) (f (Right b) (abFoldr f t cs))

abFoldr' :: (a -> t -> t) -> (b -> t -> t) -> t -> (ABList a b) -> t
abFoldr' = undefined

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

abMapToList' :: (Either a b -> t) -> ABList a b -> [t]
abMapToList' = undefined

abReverse :: ABList a b -> Either (ABList a b) (ABList b a)
abReverse = undefined

abMapLefts :: (a -> a') -> ABList a b -> ABList a' b
abMapLefts = undefined

abMapRights :: (b -> b') -> ABList a b -> ABList a b'
abMapRights = undefined

-- right-biased functor
instance Functor (ABList a) where
  fmap = abMapRights

