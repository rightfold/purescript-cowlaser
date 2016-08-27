module Data.String.CaseInsensitive
( CI(..)
, unCI
) where

import Data.String (toUpper)
import Prelude

newtype CI = CI String

unCI :: CI -> String
unCI (CI s) = s

instance eqCI :: Eq CI where
  eq (CI a) (CI b) = toUpper a == toUpper b

instance ordCI :: Ord CI where
  compare (CI a) (CI b) = toUpper a `compare` toUpper b

instance showCI :: Show CI where
  show (CI s) = "(CI " <> show s <> ")"
