module ArrayIx
  ( (!!)
  , ArrayIx
  , bottomToTop
  , downFrom
  , enumAll
  , index
  , length
  , mkArrayIx
  , mkArrayIxArrayIx
  , modifyAt
  , repeat
  , reverse
  , toArray
  , topToBottom
  , upFrom
  , updateAt
  , zipWithIx
  )
  where

import ZPrelude

import Data.Array as Array
import Data.Array (foldl, foldr, range, unsafeIndex, zipWith)
import Data.Enum (class BoundedEnum, Cardinality,
  cardinality, fromEnum, enumFromTo)
import Data.FoldableWithIndex
  (class FoldableWithIndex, foldMapWithIndexDefaultR)
import Data.FunctorWithIndex
  (class FunctorWithIndex, mapWithIndex)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (class Foldable, class Traversable,
  sequence)
import Data.TraversableWithIndex
  (class TraversableWithIndex, traverseWithIndexDefault)
import Data.Tuple (Tuple(..))

import Partial.Unsafe (unsafePartial)

-- | I got tired of circumventing all of the maybes in my code
-- | plus wondering about the performance hit for each
-- | array access. `ArrayIx` is restricted to indexing on
-- | `BoundedEnum`s that exactly match the array.
-- | Somewhat inspired by standard Haskell arrays.
-- | Fixed size `Array`s. 
-- | `Array` operations with a change in size do not belong here.
newtype ArrayIx i a = ArrayIx (Array a)

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
    <<< toArray <<< mapWithIndex Tuple
  foldlWithIndex f z = foldl (\y (Tuple i x) -> f i y x) z
    <<< toArray <<< mapWithIndex Tuple
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
      zipWith f enumAll xs


-- | A zippy `apply`. Since `ArrayIx i a` has a length fixed
-- | by the `BoundedEnum i` an `apply` producing a product
-- | is not possible.
instance applyArrayIx :: BoundedEnum i => Apply (ArrayIx i)
  where
    apply (ArrayIx fs) (ArrayIx xs) =
      ArrayIx <| zipWith ($) fs xs

-- | Unlike `ZipArray` we can have a `pure` since we know all
-- | compatible `ArrayIx`s have the same size.
instance applicativeArrayIx ::
  BoundedEnum i => Applicative (ArrayIx i) where
    pure x = repeat x

enumAll :: forall i. BoundedEnum i => Array i
enumAll = enumFromTo bottom top

toArray :: forall i a. BoundedEnum i => ArrayIx i a -> Array a
toArray (ArrayIx xs) = xs

mkArrayIx :: forall i a. BoundedEnum i =>
    Array a -> Maybe (ArrayIx i a)
mkArrayIx xs =
  if Array.length xs == unwrap (cardinality :: Cardinality i)
  then Just (ArrayIx xs)
  else Nothing

mkArrayIxArrayIx :: forall a i. BoundedEnum i =>
  Array (Array a) -> Maybe (ArrayIx i (ArrayIx i a))
mkArrayIxArrayIx xs = sequence (map mkArrayIx xs) >>= mkArrayIx

index :: forall i a. BoundedEnum i => ArrayIx i a -> i -> a
index (ArrayIx xs) i =
    unsafePartial <| unsafeIndex xs (fromEnum i)

infixl 8 index as !!

repeat :: forall i a. BoundedEnum i => a -> ArrayIx i a
repeat x = ArrayIx <|
  Array.replicate (unwrap (cardinality :: Cardinality i)) x

length :: forall i a. BoundedEnum i => ArrayIx i a -> Int
length xs = unwrap (cardinality :: Cardinality i)

reverse :: forall a i. BoundedEnum i => ArrayIx i a -> ArrayIx i a
reverse (ArrayIx xs) = ArrayIx <| Array.reverse xs

bottomToTop :: forall i. BoundedEnum i => ArrayIx i i
bottomToTop = ArrayIx <| enumFromTo bottom top

topToBottom :: forall i. BoundedEnum i => ArrayIx i i
topToBottom = ArrayIx <| enumFromTo top bottom

upFrom :: forall i. BoundedEnum i => Int -> ArrayIx i Int
upFrom x = ArrayIx <|
  range x (x + (unwrap (cardinality :: Cardinality i) - 1))

downFrom :: forall i. BoundedEnum i => Int -> ArrayIx i Int
downFrom x = ArrayIx <|
  range x (x - (unwrap (cardinality :: Cardinality i) - 1))

updateAt :: forall a i. BoundedEnum i =>
  i -> a -> ArrayIx i a -> ArrayIx i a
updateAt ix x (ArrayIx xs) = ArrayIx <| unsafePartial <|
  fromJust <| Array.updateAt (fromEnum ix) x xs

modifyAt :: forall a i. BoundedEnum i =>
  i -> (a -> a) -> ArrayIx i a -> ArrayIx i a
modifyAt ix f (ArrayIx xs) = ArrayIx <| unsafePartial <|
  fromJust <| Array.modifyAt (fromEnum ix) f xs

zipWithIx :: forall a b c i. BoundedEnum i =>
  (a -> b -> c) -> ArrayIx i a -> ArrayIx i b -> ArrayIx i c
zipWithIx f (ArrayIx xs) (ArrayIx ys) = ArrayIx <| zipWith f xs ys
 