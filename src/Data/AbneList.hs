module Data.AbneList where

import Data.AbList

infixr 5 ://
data AbneList a b = a :// AbList b a

