module GeoAlg where

-- from: http://projectivegeometricalgebra.org

import ZPrelude
import Data.Foldable
import Data.Array
import Data.Functor
import Math (sqrt, abs)
import Data.Generic.Rep
import Test.QuickCheck.Arbitrary

import Multivec
import GMult

-- Would this be easier using:
-- import Data.Vec

-- Operators /\ \/ |- -| |= =| /.\ \./ /*\ \*/

-- wedge  (Point {e1: px, e2: py, e3: pz, e4: pw}) (Point {e1: qx, e2: qy, e3: qz, e4: qw}) =
--   Line { e41: (qx * pw - px * qw), e42: (qy * pw - py * qw), e43: (qz * pw - pz * qw),
--     e23: (py * qz - pz * qy), e31: (pz * qx - px * qz), e12: (px * qy - py * qx) }

-- MultiVec = Proscalar + Vectors + Bivectors
--      + Trivectors + Antiscalar

data GeoAlg a = GeoAlg (MultivecRec a ())
    | Scalar (AscalarRec a ())
    | Point (VectorsRec a ()) | Line (BivectorsRec a ())
    | Plane (TrivectorsRec a ()) | Pseudo (ApseudoRec a ())
    | Motor (MotorRec a ()) | Flector (FlectorRec a ())

newtype DualGeo a = DualGeo (GeoAlg a)

derive instance genericGeoAlg :: Generic (GeoAlg a) _

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
    add = addGeo
    mul = geoAnti

instance ringGeoAlg :: (Eq a, Ring a) => Ring (GeoAlg a) where
  sub = subGeo

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

-- called subtract in haskell
revsub :: forall a. Ring a => a -> a -> a
revsub x y = sub y x

subGeo :: forall a. Ring a => GeoAlg a -> GeoAlg a -> GeoAlg a

subGeo (Scalar x) (Scalar y) = Scalar <| x - y
subGeo (Scalar x) (Pseudo y) = Motor <|
    motorU x bivectors0 (negate y)
subGeo (Scalar x) (Line y) = Motor <|
    motorU x (negate y) apseudo0
subGeo (Scalar x) (Motor y) = Motor <|
    inAscalar (+) x (negate y)

subGeo (Pseudo x) (Pseudo y) = Pseudo <| x - y
subGeo (Pseudo x) (Scalar y) = Motor <|
    motorU (negate y) bivectors0 x
subGeo (Pseudo x) (Line y) = Motor <|
    motorU ascalar0 (negate y) x
subGeo (Pseudo x) (Motor y) = Motor <|
    inApseudo (+) x (negate y)

subGeo (Line x) (Line y) = Line <| x - y
subGeo (Line x) (Scalar y) = Motor <|
    motorU (negate y) x apseudo0
subGeo (Line x) (Pseudo y) = Motor <|
    motorU ascalar0 x <| negate y
subGeo (Line x) (Motor y) = Motor
    (inBivectors (+) x (negate y))

subGeo (Point x) (Point y) = Point <| x - y
subGeo (Point x) (Plane y) = Flector <| flectorU x (negate y)
subGeo (Point x) (Flector y) = Flector <|
    inVectors (+) x (negate y)

subGeo (Plane x) (Plane y) = Plane <| x - y
subGeo (Plane x) (Point y) = Flector <| flectorU (negate y) x
subGeo (Plane x) (Flector y) = Flector <|
    inTrivectors (+) x (negate y)

subGeo (Motor x) (Motor y) = Motor <| x - y
subGeo (Motor x) (Scalar y) = Motor <| inAscalar revsub y x
subGeo (Motor x) (Pseudo y) = Motor <| inApseudo revsub y x
subGeo (Motor x) (Line y) = Motor <| inBivectors revsub y x

subGeo (Flector x) (Flector y) = Flector <| x - y
subGeo (Flector x) (Point y) = Flector <| inVectors revsub y x
subGeo (Flector x) (Plane y) = Flector <|
    inTrivectors revsub y x

subGeo (GeoAlg x) (GeoAlg y) = GeoAlg <| x - y
subGeo x (GeoAlg y) = GeoAlg <| expand x - y
subGeo x y = GeoAlg <| expand x - expand y

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

-- Unary operations

reverse :: forall a. Ring a => GeoAlg a -> GeoAlg a
reverse (Scalar x) = Scalar x
reverse (Pseudo x) = Pseudo x
reverse (Line x) = Line <| negate x
reverse (Point x) = Point <| x
reverse (Plane x) = Plane <| negate x
reverse (Motor x) =
    Motor <| inBivectors (negate << const) x x
reverse (Flector x) =
    Flector <| inTrivectors (negate << const) x x
reverse (GeoAlg x) = GeoAlg
    <| inBivectors (negate << const) x
    <| inTrivectors (negate << const) x x

antireverse :: forall a. Ring a => GeoAlg a -> GeoAlg a
antireverse (Scalar x) = Scalar x
antireverse (Pseudo x) = Pseudo x
antireverse (Line x) = Line <| negate x
antireverse (Point x) = Point <| negate x
antireverse (Plane x) = Plane x
antireverse (Motor x) =
    Motor <| inBivectors (negate << const) x x
antireverse (Flector x) =
    Flector <| inVectors (negate << const) x x
antireverse (GeoAlg x) = GeoAlg
    <| inBivectors (negate << const) x
    <| inVectors (negate << const) x x

rightComp :: forall a. Ring a => GeoAlg a -> GeoAlg a
rightComp (Scalar x) = Pseudo <| compAscalar id x
rightComp (Pseudo x) = Scalar <| compApseudo id x
rightComp (Line x) = Line <| compBivectors negate x
rightComp (Point x) = Plane <| compVectors id x
rightComp (Plane x) = Point <| compTrivectors negate x
rightComp (Motor x) = let xx = compMotor id x in Motor
    <| inBivectors (negate << const) xx xx
rightComp (Flector x) = let xx = compFlector id x in Flector
    <| inVectors (negate << const) xx xx
rightComp (GeoAlg x) = let xx = compMultivec id x in GeoAlg
    <| inBivectors (negate << const) xx
    <| inVectors (negate << const) xx xx

leftComp :: forall a. Ring a => GeoAlg a -> GeoAlg a
leftComp (Scalar x) = Pseudo <| compAscalar id x
leftComp (Pseudo x) = Scalar <| compApseudo id x
leftComp (Line x) = Line <| compBivectors negate x
leftComp (Point x) = Plane <| compVectors negate x
leftComp (Plane x) = Point <| compTrivectors id x
leftComp (Motor x) = let xx = compMotor id x in Motor
    <| inBivectors (negate << const) xx xx
leftComp (Flector x) = let xx = compFlector id x in Flector
    <| inTrivectors (negate << const) xx xx
leftComp (GeoAlg x) = let xx = compMultivec id x in GeoAlg
    <| inBivectors (negate << const) xx
    <| inTrivectors (negate << const) xx xx

dblComp :: forall a. Ring a => GeoAlg a -> GeoAlg a
dblComp (Scalar x) = Scalar x
dblComp (Pseudo x) = Pseudo x
dblComp (Line x) = Line <| x
dblComp (Point x) = Point <| negate x
dblComp (Plane x) = Plane <| negate x
dblComp (Motor x) = Motor <| x
dblComp (Flector x) = Flector <| negate x
dblComp (GeoAlg x) = GeoAlg <| inFlector (negate << const) x x

bulk :: forall a. Semiring a => GeoAlg a -> GeoAlg a
bulk (Scalar x) = Scalar x
bulk (Pseudo x) = Pseudo zero
bulk (Line x) = Line <| bivectors0
    { e23 = x.e23, e31 = x.e31, e12 = x.e12 }
bulk (Point x) = Point <| vectors0
    { e1 = x.e1, e2 = x.e2, e3 = x.e3 }
bulk (Plane x) = Plane <| trivectors0 { e321 = x.e321 }
bulk (Motor x) = Motor <| motor0
    { e = x.e, e23 = x.e23, e31 = x.e31, e12 = x.e12 }
bulk (Flector x) = Flector <| flector0
    { e1 = x.e1, e2 = x.e2, e3 = x.e3 }
bulk (GeoAlg x) = GeoAlg <| mvzero
    { e = x.e, e23 = x.e23, e31 = x.e31, e12 = x.e12
    , e1 = x.e1, e2 = x.e2, e3 = x.e3, e321 = x.e321}

weight :: forall a. Semiring a => GeoAlg a -> GeoAlg a
weight (Scalar x) = Scalar zero
weight (Pseudo x) = Pseudo x
weight (Line x) = Line <| bivectors0
    { e43 = x.e43, e42 = x.e42, e41 = x.e41 }
weight (Point x) = Point <| vectors0 { e4 = x.e4 }
weight (Plane x) = Plane <| trivectors0
    { e124 = x.e124, e314 = x.e314, e234 = x.e234}
weight (Motor x) = Motor <| motor0
    { e43 = x.e43, e42 = x.e42, e41 = x.e41, e1234 = x.e1234 }
weight (Flector x) = Flector <| flector0
    { e4 = x.e4, e124 = x.e124, e314 = x.e314, e234 = x.e234 }
weight (GeoAlg x) = GeoAlg <| mvzero
    { e43 = x.e43, e42 = x.e42, e41 = x.e41, e1234 = x.e1234
    , e4 = x.e4, e124 = x.e124, e314 = x.e314, e234 = x.e234 }

-- How to create a value of geoProp?
geoProp :: forall a. Eq a => Semiring a => GeoAlg a -> Boolean
geoProp (Scalar x) = true
geoProp (Pseudo x) = true
geoProp (Point x) = true
geoProp (Plane x) = true
geoProp (Line x) = geoPropMv (bwBivectors x) == zero
geoProp (Motor x) = geoPropMv (bwMotor x) == zero
geoProp (Flector x) = geoPropMv (bwFlector x) == zero
-- This is a guess, needs to be calculated.
geoProp (GeoAlg x) = geoPropMv (bwMultivec x) == zero

-- bulkNorm x = fold (+) map sq bulk x
normType :: BkWt -> Number -> GeoAlg Number
normType Bulk x = Scalar {e: x}
normType Weight x = Pseudo {e1234: x}

bwNorm :: BkWt -> GeoAlg Number -> GeoAlg Number
bwNorm bw (Scalar x) = normType bw <| abs (bwAscalar x bw)
bwNorm bw (Pseudo x) = normType bw <| abs (bwApseudo x bw)
bwNorm bw (Line x) = normType bw <| sqrtNorm (bwBivectors x) bw
bwNorm bw (Point x) = normType bw <| sqrtNorm (bwVectors x) bw
bwNorm bw (Plane x) =
    normType bw <| sqrtNorm (bwTrivectors x) bw
bwNorm bw (Motor x) = normType bw <| sqrtNorm (bwMotor x) bw
bwNorm bw (Flector x) =
    normType bw <| sqrtNorm (bwFlector x) bw
bwNorm bw (GeoAlg x) =
    normType bw <| sqrtNorm (bwMultivec x) bw

geoNorm :: GeoAlg Number -> GeoAlg Number
geoNorm (Scalar x) = normType Bulk <| abs (bwAscalar x Bulk)
geoNorm (Pseudo x) = normType Bulk <| zero
geoNorm (Line x) = normType Bulk <| geoNormMv (bwBivectors x)
geoNorm (Point x) = normType Bulk <| geoNormMv (bwVectors x)
geoNorm (Plane x) = normType Bulk <| geoNormMv (bwTrivectors x)
geoNorm (Motor x) = normType Bulk <| geoNormMv (bwMotor x)
geoNorm (Flector x) = normType Bulk <| geoNormMv (bwFlector x)
geoNorm (GeoAlg x) = normType Bulk <| geoNormMv (bwMultivec x)

-- How to create a unitized value?
isUnit :: forall a. Eq a => Semiring a => GeoAlg a -> Boolean
isUnit (Scalar x) = false
isUnit (Pseudo x) = x.e1234 == one
isUnit (Line x) = bwNormMv (bwBivectors x) Weight == one
isUnit (Point x) = bwNormMv (bwVectors x) Weight == one
isUnit (Plane x) = bwNormMv (bwTrivectors x) Weight == one
isUnit (Motor x) = bwNormMv (bwMotor x) Weight == one
isUnit (Flector x) = bwNormMv (bwFlector x) Weight == one
isUnit (GeoAlg x) = bwNormMv (bwMultivec x) Weight == one

