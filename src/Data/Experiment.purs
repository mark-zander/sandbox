module Experiment where

import ZPrelude
import Multivec

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Data.Array (drop, length, zipWith)
import Control.Plus (class Plus)
import Data.Foldable (class Foldable)
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable)
import Partial.Unsafe (unsafeCrashWith)
import Prim.TypeError (class Fail, Text)


toArray :: forall a. MultivecRec a () -> Array a
toArray x = [x.e, x.e1, x.e2, x.e3, x.e4
    , x.e23, x.e31, x.e12, x.e43, x.e42, x.e41
    , x.e321, x.e124, x.e314, x.e234, x.e1234]

toMultivec :: forall a. Ring a => Array a -> MultivecRec a ()
toMultivec [x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10
    , x11, x12, x13, x14, x15] =
    {e: x0
    , e1: x1, e2: x2, e3: x3, e4: x4
    , e23: x5, e31: x6, e12: x7
    , e43: x8, e42: x9, e41: x10
    , e321: x11, e124: x12, e314: x13, e234: x14
    , e1234: x15
    }
toMultivec _ = mvzero {e = zero}

-- data Elements a = E a | E1 a | E2 a | E3 a | E4 a
--     | E23 a | E31 a | E12 a | E43 a | E42 a | E41 a
--     | E321 a | E124 a | E314 a | E234 a | E1234 a

-- newtype Mvec a = Array (Elements a)

-- Define fixed length "Arrays"
data Arr1 a = Arr1 a
data Arr3 a = Arr3 a a a
data Arr4 a = Arr4 a a a a
data Arr6 a = Arr6 a a a a a a
data Arr8 a = Arr8 a a a a a a a a
data Arr16 a = Arr16 a a a a a a a a a a a a a a a a

-- Use phantom type parameters for element types?
-- Or does each element have it's own type?

-- data Mv1 a e = Mv1 a
-- data Mv1 a e = Mv1 e(a)

-- Use types to compute the multiply at compile time
-- Need to build up Cayley Tables at type level
-- gp :: forall a e1 e2. Ring a =>
--      Mv1 a e1 -> Mv1 a e2 -> Mv? a e?

-- Cayley table has 3 functions: mul, negate <<< mul, zero
-- Also has a return type, ie: e314


-- ZipArray

newtype ZipArray a = ZipArray (Array a)

instance showZipArray :: Show a => Show (ZipArray a) where
  show (ZipArray xs) = "(ZipArray " <> show xs <> ")"

derive instance newtypeZipArray :: Newtype (ZipArray a) _

derive newtype instance eqZipArray :: Eq a => Eq (ZipArray a)

derive newtype instance ordZipArray :: Ord a => Ord (ZipArray a)

derive newtype instance semigroupZipArray :: Semigroup (ZipArray a)

derive newtype instance monoidZipArray :: Monoid (ZipArray a)

derive newtype instance foldableZipArray :: Foldable ZipArray

derive newtype instance traversableZipArray :: Traversable ZipArray

derive newtype instance functorZipArray :: Functor ZipArray

instance applyZipArray :: Apply ZipArray where
  apply (ZipArray fs) (ZipArray xs) = ZipArray (zipWith ($) fs xs)

-- How to rewrite?
-- instance applicativeZipArray :: Applicative ZipArray where
--   pure = ZipArray <<< repeat

instance altZipArray :: Alt ZipArray where
  alt (ZipArray xs) (ZipArray ys) = ZipArray $ xs <> drop (length xs) ys

instance plusZipArray :: Plus ZipArray where
  empty = mempty

-- Requires Applicative/pure
-- instance alternativeZipArray :: Alternative ZipArray


-- Tuples using records

type T01 a r = (t01:: a | r)
type T02 a b r = T01 a (t02:: b | r)
type T03 a b c r = T02 a b (t03:: c | r)

type Tuple01 a r = Record (T01 a r)
type Tuple02 a b r = Record (T02 a b r)
type Tuple03 a b c r = Record (T03 a b c r)

-- mkT01 :: forall a r. a -> Tuple01 a r
-- mkT01 a r = {t01: a} 

-- mkT02 :: forall a b r. a -> b -> Tuple02 a b ()
-- mkT02 a b = (mkT01 a) <> {t02: b}
-- mkT02 a b = {t02: b, (mkT01 a)}

data E a = E {e:: a}
