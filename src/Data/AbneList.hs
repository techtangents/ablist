module Data.AbneList where

import Data.AbList

infixr 5 ://
data AbneList a b = a :// AbList b a

-- test
abneHead :: AbneList a b -> a
abneHead (a :// _) = a

-- test
abneTail :: AbneList a b -> AbList b a
abneTail (_ :// as) = as

-- fold
-- reduce
-- reverse
-- map
-- functor