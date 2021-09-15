module GeoAlg where

-- from: http://projectivegeometricalgebra.org

import ZPrelude
import Math (abs)

import Multivec
import GeoAlgType

-- Would this be easier using:
-- import Data.Vec


-- wedge  (Point {e1: px, e2: py, e3: pz, e4: pw}) (Point {e1: qx, e2: qy, e3: qz, e4: qw}) =
--   Line { e41: (qx * pw - px * qw), e42: (qy * pw - py * qw), e43: (qz * pw - pz * qw),
--     e23: (py * qz - pz * qy), e31: (pz * qx - px * qz), e12: (px * qy - py * qx) }

-- MultiVec = Proscalar + Vectors + Bivectors
--      + Trivectors + Antiscalar


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

