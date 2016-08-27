module Data.String.CaseInsensitive
( CI(..)
) where

import Data.String (toUpper)
import Prelude

newtype CI = CI String

instance eqCI :: Eq CI where
  eq (CI a) (CI b) = toUpper a == toUpper b

instance ordCI :: Ord CI where
  compare (CI a) (CI b) = toUpper a `compare` toUpper b
