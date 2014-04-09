module Data.ABList (
  ABList(..),
  abFoldr,
  abFoldr',
  abToListEither,
  abFromListEither,
  aaToList,
  aaFromList,
  abHead,
  abTail
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
aaToList = undefined

aaFromList :: [a] -> ABList a a
aaFromList = undefined

abFoldr :: ((Either a b) -> t -> t) -> t -> (ABList a b) -> t
abFoldr = undefined

abFoldr' :: (a -> t -> t) -> (b -> t -> t) -> t -> (ABList a b) -> t
abFoldr' = undefined

abZip :: [a] -> [b] -> ABList a b
abZip = undefined

abZipPairs :: [(a, b)] -> ABList a b
abZipPairs = undefined

abMap :: (a -> a') -> (b -> b') -> ABList a b -> ABList a' b'
abMap = undefined

abMapToList :: (a -> t) -> (b -> t) -> ABList a b -> [t]
abMapToList = undefined

abMapToList' :: (Either a b -> t) -> ABList a b -> [t]
abMapToList' = undefined

abUnzip :: ABList a b -> ([a], [b])
abUnzip = undefined

abUnzip' :: ABList a b -> (a, [b], [a])
abUnzip' = undefined

abUnzip'' :: ABList a b -> (a, [(b, a)])
abUnzip'' = undefined

abReverse :: ABList a b -> Either (ABList a b) (ABList b a)
abReverse = undefined

abMapLefts :: (a -> a') -> ABList a b -> ABList a' b
abMapLefts = undefined

abMapRights :: (b -> b') -> ABList a b -> ABList a b'
abMapRights = undefined


-- right-biased functor
instance Functor (ABList a) where
  fmap = abMapRights



