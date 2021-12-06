module ZipArray
  ( ZipArray
  )
  where

import ZPrelude

import Control.Alt (class Alt)
import Control.Plus (class Plus)
import Data.Array (zipWith, drop, length)
import Data.Foldable (class Foldable)
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable)

-- | `ZipArray` is modeled after `ZipList` except with an
-- | underlying `Array` rather than a `List`. Since there
-- | is no infinite `Array` a `pure` function in
-- | `Applicative` is not possible.

newtype ZipArray a = ZipArray (Array a)

instance showZipArray :: Show a => Show (ZipArray a) where
  show (ZipArray xs) = "(ZipArray " <> show xs <> ")"

derive instance newtypeZipArray :: Newtype (ZipArray a) _

derive instance eqZipArray :: Eq a => Eq (ZipArray a)

derive instance ordZipArray :: Ord a => Ord (ZipArray a)

derive newtype instance semigroupZipArray ::
    Semigroup (ZipArray a)

derive newtype instance monoidZipArray :: Monoid (ZipArray a)

derive newtype instance foldableZipArray :: Foldable ZipArray

derive newtype instance traversableZipArray ::
    Traversable ZipArray

derive newtype instance functorZipArray :: Functor ZipArray

instance applyZipArray :: Apply ZipArray where
  apply (ZipArray fs) (ZipArray xs) =
    ZipArray (zipWith ($) fs xs)

-- ZipArray is impure.
-- instance applicativeZipArray :: Applicative ZipArray where
--   pure = ZipArray <<< repeat

-- Taken from `ZipList`
instance altZipArray :: Alt ZipArray where
  alt (ZipArray xs) (ZipArray ys) =
    ZipArray $ xs <> drop (length xs) ys

instance plusZipArray :: Plus ZipArray where
  empty = mempty

-- Requires Applicative/pure
-- instance alternativeZipArray :: Alternative ZipArray
