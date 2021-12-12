module ArrayIx
  ( ArrayIx
  , index
  , mkArrayIx
  )
  where

import ZPrelude

import Data.Array (foldl, foldr, length, unsafeIndex, zipWith)
import Data.Enum (class BoundedEnum, Cardinality,
  cardinality, fromEnum, enumFromTo)
import Data.FoldableWithIndex
  (class FoldableWithIndex, foldMapWithIndexDefaultR)
import Data.FunctorWithIndex
  (class FunctorWithIndex, mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (class Foldable, class Traversable)
import Data.TraversableWithIndex
  (class TraversableWithIndex, traverseWithIndexDefault)
import Data.Tuple (Tuple(..))

import Partial.Unsafe (unsafePartial)

-- | I got tired of circumventing all of the maybes in my code
-- | plus wondering about the performance hit for each
-- | array access. `ArrayIx` is restricted to indexing on
-- | `BoundedEnum`s that exactly match the array.
-- | Somewhat inspired by standard Haskell arrays.
newtype ArrayIx i a = ArrayIx (Array a)

allFromTo :: forall i. BoundedEnum i => Array i
allFromTo = enumFromTo bottom top

undo :: forall i a. BoundedEnum i => ArrayIx i a -> Array a
undo (ArrayIx xs) = xs

derive instance newtypeArrayIx ::
  BoundedEnum i => Newtype (ArrayIx i a) _

instance showArrayIx :: Show a => Show (ArrayIx i a) where
  show (ArrayIx xs) = "(ArrayIx " <> show xs <> ")"

derive instance eqArrayIx ::
  (BoundedEnum i, Eq a) => Eq (ArrayIx i a)

derive instance ordArrayIx ::
  (BoundedEnum i, Ord a) => Ord (ArrayIx i a)

derive newtype instance foldableArrayIx ::
  BoundedEnum i => Foldable (ArrayIx i)

instance foldableWithIndexArrayix ::
  BoundedEnum i => FoldableWithIndex i (ArrayIx i) where
  foldrWithIndex f z = foldr (\(Tuple i x) y -> f i x y) z
    <<< undo <<< mapWithIndex Tuple
  foldlWithIndex f z = foldl (\y (Tuple i x) -> f i y x) z
    <<< undo <<< mapWithIndex Tuple
  foldMapWithIndex = foldMapWithIndexDefaultR

derive newtype instance traversableArrayIx ::
  BoundedEnum i => Traversable (ArrayIx i)

instance traversableWithIndexArrayIx ::
  BoundedEnum i => TraversableWithIndex i (ArrayIx i) where
  traverseWithIndex = traverseWithIndexDefault

derive newtype instance functorArrayIx ::
  BoundedEnum i => Functor (ArrayIx i)

instance functorWithIndexArrayIx ::
  BoundedEnum i => FunctorWithIndex i (ArrayIx i) where
    mapWithIndex f (ArrayIx xs) = ArrayIx <|
      zipWith f allFromTo xs


-- | A zippy `apply`. Since `ArrayIx i a` has a length fixed
-- | by the `BoundedEnum i` an `apply` producing a product
-- | is not possible.
instance applyArrayIx :: BoundedEnum i => Apply (ArrayIx i)
  where
    apply (ArrayIx fs) (ArrayIx xs) =
      ArrayIx <| zipWith ($) fs xs



mkArrayIx :: forall i a. BoundedEnum i =>
    Array a -> Maybe (ArrayIx i a)
mkArrayIx xs =
  if length xs == unwrap (cardinality :: Cardinality i)
  then Just (ArrayIx xs)
  else Nothing

index :: forall i a. BoundedEnum i => ArrayIx i a -> i -> a
index (ArrayIx xs) i =
    unsafePartial <| unsafeIndex xs (fromEnum i)

