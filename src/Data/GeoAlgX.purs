-- Experimental Geometric Algebra
-- With simplified add and subtract
-- How to do multiply?
module GeoAlgX where

import ZPrelude
import Math (abs)
import Data.Enum
import Data.Maybe
import Data.Generic.Rep
import Data.Generic.Rep.Bounded
import Data.Generic.Rep.Enum
import Data.Array as Array
import Data.Newtype
import Partial.Unsafe (unsafePartial)

import Test.QuickCheck.Arbitrary
    (class Arbitrary, genericArbitrary)

import Multivec

-- Operators /\ \/ |- -| |= =| /.\ \./ /*\ \*/

-- infix 7 geoProd as /.\
-- infix 7 geoAnti as \./
-- infix 7 extProd as /\
-- infix 7 extAnti as \/

data Element = E | E1 | E2 | E3 | E4 |
    E23 | E31 | E12 | E43 | E42 | E41 |
    E321 | E124 | E314 | E234 | E1234

derive instance eqElement :: Eq Element
derive instance ordElement :: Ord Element
derive instance genericElement :: Generic Element _
instance boundedElement :: Bounded Element where
  bottom = genericBottom   
  top = genericTop
instance enumElement :: Enum Element where
  succ = genericSucc
  pred = genericPred
instance boundedenumElement :: BoundedEnum Element where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

newtype GeoAlg a = GeoAlg (Array a)

infixl 8 index as !

index :: forall a. Array a -> Element -> a
index a i = unsafePartial <| Array.unsafeIndex a (fromEnum i)

mkGeoAlg :: forall a. Array a -> Maybe (GeoAlg a)
mkGeoAlg a =
    if Array.length a ==
      unwrap (cardinality :: Cardinality Element)
    then Just (GeoAlg a)
    else Nothing

-- class (Functor f, Enum a) <= Indexable f a b where

--     cardinality = Cardinality 5
--     toEnum 0 = Just E
--     toEnum 1 = Just E1
--     toEnum 2 = Just E2
--     toEnum 3 = Just E3
--     toEnum 4 = Just E4
--     toEnum _ = Nothing
--     fromEnum E = 0
--     fromEnum E1 = 1
--     fromEnum E2 = 2
--     fromEnum E3 = 3
--     fromEnum E4 = 4

-- newtype GeoAlg a = GeoAlg (MultivecRec a ())

-- -- Needed by arbitrary.
-- derive instance genericGeoAlg :: Generic (GeoAlg a) _

-- -- Used for testing.
-- instance arbitraryGeoAlg ::
--         (Generic (GeoAlg a) rep, Arbitrary rep) =>
--         Arbitrary (GeoAlg a) where
--     arbitrary = genericArbitrary

-- instance eqGeoAlg :: (Eq a, Ring a) => Eq (GeoAlg a) where
--     eq (GeoAlg x) (GeoAlg y) = x == y

-- instance semiringGeoAlg ::
--     (Eq a, Ring a) => Semiring (GeoAlg a) where
--     zero = GeoAlg mvzero
--     one = GeoAlg (mvzero { e1234 = 1 })
--     add (GeoAlg x) (GeoAlg y) = GeoAlg (x + y)
--     mul = geoAnti

-- instance ringGeoAlg :: (Eq a, Ring a) => Ring (GeoAlg a) where
--     sub (GeoAlg x) (GeoAlg y) = GeoAlg (x - y)

