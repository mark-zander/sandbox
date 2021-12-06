module Zip1Array
  ( Zip1Array
  )
  where

import ZPrelude

import Data.Array (find, zipWith)
import Data.Maybe (maybe)

-- | `Zip1Array` is modeled after `Zip1List` except with an
-- | underlying `Array` rather than a `List`. Since there
-- | is no infinite `Array` a `pure` function is implemented
-- | using a `Zip1Pure` data constructor.

data Zip1Array a
  = Zip1Array (Array a)
  | Zip1Pure a

-- An attempt at rank 2 polymorphism but how do you pass
-- a class member function like `eq` or `compare`.
-- cmpz :: forall a b. Eq a => (forall c. c -> c -> b) ->
--   b -> Zip1Array a -> Zip1Array a -> b
-- cmpz f def (Zip1Array xs) (Zip1Array ys) = f xs ys
-- cmpz f def (Zip1Array xs) (Zip1Pure y) =
--   maybe def (flip f y) (find (notEq y) xs)
-- cmpz f def (Zip1Pure x) (Zip1Array ys) =
--   maybe def (f x) (find (notEq x) ys)
-- cmpz f def (Zip1Pure x) (Zip1Pure y) = f x y

instance showZip1Array :: Show a => Show (Zip1Array a) where
  show (Zip1Array xs) = "(Zip1Array " <> show xs <> ")"
  show (Zip1Pure x) = "(Zip1Pure " <> show x <> ")"

instance eqZip1Array :: Eq a => Eq (Zip1Array a) where
  eq (Zip1Array xs) (Zip1Array ys) = eq xs ys
  eq (Zip1Array xs) (Zip1Pure y) =
    maybe true (const false) (find (notEq y) xs)
  eq (Zip1Pure x) (Zip1Array ys) =
    maybe true (const false) (find (notEq x) ys)
  eq (Zip1Pure x) (Zip1Pure y) = eq x y

instance ordZip1Array :: Ord a => Ord (Zip1Array a) where
  compare (Zip1Array xs) (Zip1Array ys) = compare xs ys
  compare (Zip1Array xs) (Zip1Pure y) =
    maybe EQ (flip compare y) (find (notEq y) xs)
  compare (Zip1Pure x) (Zip1Array ys) =
    maybe EQ (compare x) (find (notEq x) ys)
  compare (Zip1Pure x) (Zip1Pure y) = compare x y

-- See `Zip2Array` for a possible implementation that
-- includes these instances.
-- instance semigroupZip1Array :: Semigroup (Zip1Array a)
-- instance monoidZip1Array :: Monoid (Zip1Array a)

-- With a lazy/infinite version of pure these may be possible
-- instance foldableZip1Array :: Foldable Zip1Array
-- instance traversableZip1Array :: Traversable Zip1Array

instance functorZip1Array :: Functor Zip1Array where
  map f (Zip1Array xs) = Zip1Array <| map f xs
  map f (Zip1Pure x) = Zip1Pure <| f x

instance applyZip1Array :: Apply Zip1Array where
  apply (Zip1Array fs) (Zip1Array xs) =
    Zip1Array (zipWith ($) fs xs)
  apply (Zip1Array fs) (Zip1Pure x) =
    Zip1Array <| map (\f -> f x) fs
  apply (Zip1Pure f) (Zip1Array xs) = Zip1Array <| map f xs
  apply (Zip1Pure f) (Zip1Pure x) = Zip1Pure <| f x

instance applicativeZip1Array :: Applicative Zip1Array where
  pure = Zip1Pure

-- Need `Semigroup` to implement these.
-- instance altZip1Array :: Alt Zip1Array where
-- instance plusZip1Array :: Plus Zip1Array where
-- instance alternativeZip1Array :: Alternative Zip1Array
