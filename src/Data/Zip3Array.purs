module Zip3Array
  ( Zip3Array(..)
  )
  where

import ZPrelude

import Data.Array (zipWith, drop, length, find, last)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)


-- Zips 2 Arrays where the shorter array is extended by
-- its last element to match the longer array.
zip3With :: forall a b c. (a -> b -> c) ->
  Array a -> Array b -> Array c
zip3With f xs ys = zipWith f xs ys <>
    zz (last xs) (last ys) (length xs) (length ys)
  where
  zz (Just xlast) (Just ylast) xlen ylen
    | xlen < ylen = map (f xlast) (drop (xlen) ys)
    | xlen > ylen = map (\x -> f x ylast) (drop (ylen) xs)
    | otherwise = []
  zz _ _ _ _ = []

-- Pretty sure this makes for lawless instances.
-- Which ones? Why?
newtype Zip3Array a = Zip3Array (Array a)

instance showZip3Array :: Show a => Show (Zip3Array a) where
  show (Zip3Array xs) = "(Zip3Array " <> show xs <> ")"

derive instance newtypeZip3Array :: Newtype (Zip3Array a) _

instance eqZip3Array :: Eq a => Eq (Zip3Array a) where
  eq (Zip3Array xs) (Zip3Array ys) =
    maybe true id (find (_ == false) (zip3With (==) xs ys))

instance ordZip3Array :: Ord a => Ord (Zip3Array a) where
  compare (Zip3Array xs) (Zip3Array ys) =
    maybe EQ id <| find (_ /= EQ) <| zip3With compare xs ys

-- derive newtype instance
--   semigroupZip3Array :: Semigroup (Zip3Array a)

-- derive newtype instance
--   monoidZip3Array :: Monoid (Zip3Array a)

-- derive newtype instance
--   foldableZip3Array :: Foldable Zip3Array

-- derive newtype instance
--   traversableZip3Array :: Traversable Zip3Array

-- derive newtype instance functorZip3Array :: Functor Zip3Array

-- instance applyZip3Array :: Apply Zip3Array where
--   apply (Zip3Array fs) (Zip3Array xs) =
--     Zip3Array (zip3With ($) fs xs)

-- instance applicativeZip3Array :: Applicative Zip3Array where
--   pure = Zip3Array <<< singleton

-- instance altZipArray :: Alt ZipArray where
--   alt (ZipArray xs) (ZipArray ys) = ZipArray $ xs <> drop (length xs) ys

-- instance plusZipArray :: Plus ZipArray where
--   empty = mempty

-- instance applyZip3Array :: Apply Zip3Array where
--   apply (Zip3Pure f) (Zip3Pure x) = Zip3Pure <| f x
--   apply (Zip3Pure f) (Zip3Array xs) = Zip3Array <| map f xs
--   apply (Zip3Array fs) (Zip3Pure x) =
--     Zip3Array <| map (\f -> f x) fs
--   apply (Zip3Array fs) (Zip3Array xs) =
--     Zip3Array (zipWith ($) fs xs)

-- zip3With f x y = zipWith f x y <>
--   if length x < length y then map (f (last x)) (drop (length x) y)
--   else if y < x then map (\z -> f z (last y)) (drop (length y) x)
--   else []

-- zip3With f x y = zipWith f x y <>
--   do
--     x1 = last x
--     y1 = last y
--     if length x < length y then
--       map (f x1) (drop (length x1)) y
--     else if length x > length y then
--       map (\z -> f z (last y)) (drop (length y)) x

-- zip3With f x y = zipWith f x y <>
--   case last x of
--   Nothing -> []
--   Just xlast -> case last y of
--     Nothing -> []
--     Just ylast
--       | xlen < ylen -> map (f xlast) (drop (ylen) x)
--       | xlen > ylen -> map (f ylast) (drop (xlen) y)
--       | otherwise -> []
--   where
--     xlen = length x
--     ylen = length y
