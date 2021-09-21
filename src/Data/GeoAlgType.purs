-- | Geometric algebra type and associated instances and
-- | operations.
module GeoAlgType where

import ZPrelude

import Data.Generic.Rep (class Generic)
import Test.QuickCheck.Arbitrary
    (class Arbitrary, genericArbitrary)

import Multivec
import GMult

-- Operators /\ \/ |- -| |= =| /.\ \./ /*\ \*/

-- All of the intances have to be with the types so these are
-- isolated to keep file size down.

-- | A complete GeoAlg and all of it subcomponents.
-- | Subcomponents assume unused values are == zero.
data GeoAlg a = GeoAlg (MultivecRec a ())
    | Scalar (AscalarRec a ())
    | Point (VectorsRec a ()) | Line (BivectorsRec a ())
    | Plane (TrivectorsRec a ()) | Pseudo (ApseudoRec a ())
    | Motor (MotorRec a ()) | Flector (FlectorRec a ())

-- | Dual is same except for the multiply used.
newtype DualGeo a = DualGeo (GeoAlg a)

-- Needed by arbitrary.
derive instance genericGeoAlg :: Generic (GeoAlg a) _

-- Used for testing.
instance arbitraryGeoAlg ::
        (Generic (GeoAlg a) rep, Arbitrary rep) =>
        Arbitrary (GeoAlg a) where
    arbitrary = genericArbitrary

instance eqGeoAlg :: (Eq a, Ring a) => Eq (GeoAlg a) where
    eq (Scalar x) (Scalar y) = x == y
    eq (Pseudo x) (Pseudo y) = x == y
    eq (Point x) (Point y) = x == y
    eq (Line x) (Line y) = x == y
    eq (Plane x) (Plane y) = x == y
    eq (Motor x) (Motor y) = x == y
    eq (Flector x) (Flector y) = x == y
    eq (GeoAlg x) (GeoAlg y) = x == y
    eq x y = expand x == expand y

-- | Order of elements in subcomponents is different than in
-- | Multivec.
showPlane ::
    forall a r. Show a => TrivectorsRec a r -> String
showPlane x =
    " e234: " <> show x.e234 <>
    ", e314: " <> show x.e314 <>
    ", e124: " <> show x.e124 <>
    ", e321: " <> show x.e321

showLine1 ::
    forall a r. Show a => BivectorsRec a r -> String
showLine1 x =
    " e41: " <> show x.e41 <>
    ", e42: " <> show x.e42 <>
    ", e43: " <> show x.e43

showLine2 ::
    forall a r. Show a => BivectorsRec a r -> String
showLine2 x =
    " e23: " <> show x.e23 <>
    ", e31: " <> show x.e31 <>
    ", e12: " <> show x.e12

showLine ::
    forall a r. Show a => BivectorsRec a r -> String
showLine x = showLine1 x <> "," <> showLine2 x

showMotor :: forall a r. Show a => MotorRec a r -> String
showMotor x = showLine1 x <> "," <> showApseudoRec x <>
    ", " <> showLine2 x <> ", " <> showAscalarRec x

showFlector :: forall a r. Show a => FlectorRec a r -> String
showFlector x = showVectorsRec x <> ", " <> showPlane x

-- Force the element order to be the same as in code
instance showGeoAlg :: Show a => Show (GeoAlg a) where
    show (Scalar x) = "(Scalar " <> show x <> ")"
    show (Pseudo x) = "(Pseudo " <> show x <> ")"
    show (Point x) = "(Point " <> show x <> ")"
    show (Plane x) = "(Plane {" <> showPlane x <> "})"
    show (Line x) = "(Line {" <> showLine x <> "})"
    show (Motor x) = "(Motor {" <> showMotor x <> "})"
    show (Flector x) = "(Flector {" <> showFlector x <> "})"
    show (GeoAlg x) = "(GeoAlg " <>
        showMotor x <> ", " <> showFlector x <> "})"

-- Contracts zero to a scalar, should this be a Pseudo?
-- Depends on choice of multiply?
-- Or it might not really matter.
-- Contraction to subcomponent if possible
contract :: forall a. Eq a => Semiring a =>
    GeoAlg a -> GeoAlg a
contract m@(Motor x) =
    case subMotor x of
    0 -> Scalar <| exAscalar id x
    1 -> Scalar <| exAscalar id x
    2 -> Line <| exBivectors id x
    4 -> Pseudo <| exApseudo id x
    _ -> m
contract f@(Flector x) =
    case subFlector x of
    8 -> Point <| exVectors id x
    16 -> Plane <| exTrivectors id x
    _ -> f
contract g@(GeoAlg x) = let sub = subMultivec x in
    case sub of
    0 -> Scalar <| exAscalar id x
    1 -> Scalar <| exAscalar id x
    2 -> Line <| exBivectors id x
    4 -> Pseudo <| exApseudo id x
    8 -> Point <| exVectors id x
    16 -> Plane <| exTrivectors id x
    24 -> Flector <| exFlector id x
    _ -> if sub > 8 then g else Motor <| exMotor id x
contract x = x

-- expand a subvector to a full Multivector
expand :: forall a. Semiring a => GeoAlg a -> MultivecRec a ()
expand (Scalar x) = inAscalar const x mvzero
expand (Pseudo x) = inApseudo const x mvzero
expand (Point x) = inVectors const x mvzero
expand (Line x) = inBivectors const x mvzero
expand (Plane x) = inTrivectors const x mvzero
expand (Motor x) = inMotor const x mvzero
expand (Flector x) = inFlector const x mvzero
expand (GeoAlg x) = x

instance semiringGeoAlg ::
    (Eq a, Ring a) => Semiring (GeoAlg a) where
    zero = Pseudo zero
    one = Pseudo one
    add x = contract << addGeo x
    mul = geoAnti

instance ringGeoAlg :: (Eq a, Ring a) => Ring (GeoAlg a) where
  sub x = contract << subGeo x

addGeo :: forall a. Semiring a => GeoAlg a -> GeoAlg a -> GeoAlg a
addGeo (Scalar x) (Scalar y) = Scalar <| x + y
addGeo (Scalar x) (Pseudo y) = Motor <| motorU x bivectors0 y
addGeo (Scalar x) (Line y) = Motor <| motorU x y apseudo0
addGeo (Scalar x) (Motor y) = Motor <| inAscalar (+) x y
addGeo (Scalar x) (GeoAlg y) = GeoAlg <| inAscalar (+) x y

addGeo (Pseudo x) (Pseudo y) = Pseudo <| x + y
addGeo (Pseudo x) (Scalar y) = Motor <| motorU y bivectors0 x
addGeo (Pseudo x) (Line y) = Motor <| motorU ascalar0 y x
addGeo (Pseudo x) (Motor y) = Motor <| inApseudo (+) x y
addGeo (Pseudo x) (GeoAlg y) = GeoAlg <| inApseudo (+) x y

addGeo (Line x) (Line y) = Line <| x + y
addGeo (Line x) (Scalar y) = Motor <| motorU y x apseudo0
addGeo (Line x) (Pseudo y) = Motor <| motorU ascalar0 x y
addGeo (Line x) (Motor y) = Motor <| inBivectors (+) x y
addGeo (Line x) (GeoAlg y) = GeoAlg <| inBivectors (+) x y

addGeo (Point x) (Point y) = Point <| x + y
addGeo (Point x) (Plane y) = Flector <| flectorU x y
addGeo (Point x) (Flector y) = Flector <| inVectors (+) x y
addGeo (Point x) (GeoAlg y) = GeoAlg <| inVectors (+) x y

addGeo (Plane x) (Plane y) = Plane <| x + y
addGeo (Plane x) (Point y) = Flector <| flectorU y x
addGeo (Plane x) (Flector y) = Flector <| inTrivectors (+) x y
addGeo (Plane x) (GeoAlg y) = GeoAlg <| inTrivectors (+) x y

addGeo (Motor x) (Motor y) = Motor <| x + y
addGeo (Motor x) (Scalar y) = Motor <| inAscalar (+) y x
addGeo (Motor x) (Pseudo y) = Motor <| inApseudo (+) y x
addGeo (Motor x) (Line y) = Motor <| inBivectors (+) y x
addGeo (Motor x) (GeoAlg y) = GeoAlg <| inMotor (+) x y

addGeo (Flector x) (Flector y) = Flector <| x + y
addGeo (Flector x) (Point y) = Flector <| inVectors (+) y x
addGeo (Flector x) (Plane y) = Flector <| inTrivectors (+) y x
addGeo (Flector x) (GeoAlg y) = GeoAlg <| inFlector (+) x y

addGeo (GeoAlg x) (GeoAlg y) = GeoAlg <| x + y
addGeo x y = GeoAlg <| expand x + expand y

negGeo :: forall a. Ring a => GeoAlg a -> GeoAlg a
negGeo (Scalar x) = Scalar (negate x)
negGeo (Pseudo x) = Pseudo (negate x)
negGeo (Line x) = Line (negate x)
negGeo (Point x) = Point (negate x)
negGeo (Plane x) = Plane (negate x)
negGeo (Motor x) = Motor (negate x)
negGeo (Flector x) = Flector (negate x)
negGeo (GeoAlg x) = GeoAlg (negate x)

subGeo :: forall a. Ring a => GeoAlg a -> GeoAlg a -> GeoAlg a
subGeo x (Scalar y) = addGeo x <| Scalar (negate y)
subGeo x (Pseudo y) = addGeo x <| Pseudo (negate y)
subGeo x (Line y) = addGeo x <| Line (negate y)
subGeo x (Point y) = addGeo x <| Point (negate y)
subGeo x (Plane y) = addGeo x <| Plane (negate y)
subGeo x (Motor y) = addGeo x <| Motor (negate y)
subGeo x (Flector y) = addGeo x <| Flector (negate y)
subGeo x (GeoAlg y) = addGeo x <| GeoAlg (negate y)

-- multGeo :: forall a. Ring a => GeoAlg a -> GeoAlg a -> GeoAlg a
-- multGeo f (Scalar x) (Scalar y) = f x y

-- Geometric products need component type of Ring as
-- multiplies are computed with subtractions.
geoProd :: forall a. Eq a => Ring a =>
    GeoAlg a -> GeoAlg a -> GeoAlg a
-- geoProd x y = GeoAlg <| gp (expand x) (expand y)
geoProd x y = contract <| GeoAlg <| gp (expand x) (expand y)

extProd :: forall a. Eq a => Ring a =>
    GeoAlg a -> GeoAlg a -> GeoAlg a
extProd x y = contract <| GeoAlg <| xp (expand x) (expand y)

geoAnti :: forall a. Eq a => Ring a =>
    GeoAlg a -> GeoAlg a -> GeoAlg a
geoAnti x y = contract <| GeoAlg <| gap (expand x) (expand y)

extAnti :: forall a. Eq a => Ring a =>
    GeoAlg a -> GeoAlg a -> GeoAlg a
extAnti x y = contract <| GeoAlg <| xap (expand x) (expand y)
