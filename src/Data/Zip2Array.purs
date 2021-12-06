module Zip2Array
  ( Zip2Array
  )
  where

import ZPrelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Plus (class Plus)
import Data.Array (zipWith, drop, length, find)
import Data.Maybe (maybe)


-- | `Zip2Array` attempts to extend `ZipArray` with a
-- | `pure` function in `Applicative`. It improves on
-- | `Zip1Array` with an extra argument to the `Zip2Pure`
-- | data constructor which allows for an implementation of
-- | `Semigroup`.
-- | There is no `Foldable` or `Traversable`. Maybe an
-- | lazy data type in the second argument of `Zip2Pure`?
-- |
-- | `ZipArray` is much simpler and recommended.

data Zip2Array a =
  Zip2Pure (Array a) a | Zip2Array (Array a)

zip2With :: forall a b c. (a -> b -> c) ->
  Zip2Array a -> Zip2Array b -> Zip2Array c
zip2With f (Zip2Array xs) (Zip2Array ys) =
  Zip2Array <| zipWith f xs ys
zip2With f (Zip2Array xs) (Zip2Pure ys ylast) =
  Zip2Array <| zipWith f xs ys <> zz (length xs) (length ys)
  where
  zz xlen ylen
    | xlen > ylen = map (\x -> f x ylast) (drop (ylen) xs)
    | otherwise = []
zip2With f (Zip2Pure xs xlast) (Zip2Array ys) =
  Zip2Array <| zipWith f xs ys <> zz (length xs) (length ys)
  where
  zz xlen ylen
    | xlen < ylen = map (f xlast) (drop (xlen) ys)
    | otherwise = []
zip2With f (Zip2Pure xs xlast) (Zip2Pure ys ylast) =
  Zip2Pure (zipWith f xs ys <>
    zz (length xs) (length ys)) (f xlast ylast)
  where
  zz xlen ylen
    | xlen < ylen = map (f xlast) (drop (xlen) ys)
    | xlen > ylen = map (\x -> f x ylast) (drop (ylen) xs)
    | otherwise = []

undo :: forall a. Zip2Array a -> Array a
undo (Zip2Array xx) = xx
undo (Zip2Pure xx x) = xx <> [x]


instance showZip2Array :: Show a => Show (Zip2Array a) where
  show (Zip2Pure xs xlast) =
    "(Zip2Pure " <> show xs <> show xlast <> ")"
  show (Zip2Array xs) = "(Zip2Array " <> show xs <> ")"

instance eqZip2Array :: Eq a => Eq (Zip2Array a) where
  eq xs ys = maybe true id <|
    find (_ == false) <| undo <| zip2With eq xs ys

instance ordZip2Array :: Ord a => Ord (Zip2Array a) where
  compare xs ys = maybe EQ id <|
    find (_ /= EQ) <| undo <| zip2With compare xs ys

instance semigroupZip2Array :: Semigroup (Zip2Array a) where
  append x@(Zip2Pure _ _) _ = x
  append (Zip2Array xs) (Zip2Pure ys y) =
    Zip2Pure (xs <> ys) y
  append (Zip2Array xs) (Zip2Array ys) =
    Zip2Array (xs <> ys)

instance monoidZip2Array :: Monoid (Zip2Array a) where
  mempty = Zip2Array []

-- With a lazy/infinite version of pure these may be possible
-- instance foldableZip2Array :: Foldable Zip2Array where
-- instance traversableZipArray :: Traversable ZipArray

instance functorZip2Array :: Functor Zip2Array where
  map f (Zip2Pure xs x) = Zip2Pure (map f xs) (f x)
  map f (Zip2Array xs) = Zip2Array <| map f xs

instance applyZip2Array :: Apply Zip2Array where
  apply fs xs = zip2With ($) fs xs

instance applicativeZip2Array :: Applicative Zip2Array where
  pure = Zip2Pure []

-- Taken from `ZipList`
instance altZip2Array :: Alt Zip2Array where
  alt x@(Zip2Pure _ _) _ = x
  alt (Zip2Array xs) (Zip2Pure ys y) =
    Zip2Pure (xs <> drop (length xs) ys) y
  alt (Zip2Array xs) (Zip2Array ys) =
    Zip2Array $ xs <> drop (length xs) ys

instance plusZip2Array :: Plus Zip2Array where
  empty = mempty

instance alternativeZip2Array :: Alternative Zip2Array
