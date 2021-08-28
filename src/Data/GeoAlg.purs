module GeoAlg where

-- from: http://projectivegeometricalgebra.org

import ZPrelude
import Data.Foldable
import Data.Array
import Data.Functor

import Multivec

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
showMotor x = showLine1 x <> "," <> showApseudo x <> ", " <>
    showLine2 x <> ", " <> showAscalar x

showFlector :: forall a r. Show a => FlectorRec a r -> String
showFlector x = showVectorsRec x <> ", " <> showPlane x

-- Force the element order to be the same as in code
instance showGeoAlg :: Show a => Show (GeoAlg a) where
    show (Scalar x) = "(Scalar " <> show x <> ")"
    show (Pseudo x) = "(Pseudo " <> show x <> ")"
    show (Point x) = "(Point " <> show x <> ")"
    show (Plane x) = "(Plane {" <> showPlane x <> "})"
    show (Line x) = "(Line {" <> showLine x <> "})"
    show (Motor x) = "(Motor {" <> showMotor x <> ", " <>
        showLine x <> ", " <> showAscalar x <> "})"
    show (Flector x) = "(Flector {" <> showFlector x <> "})"
    show (GeoAlg x) = "(GeoAlg " <>
        showMotor x <> ", " <> showFlector x <> "})"

-- Contraction to subcomponent if possible
contract :: forall a. Eq a => Ring a => GeoAlg a -> GeoAlg a
contract m@(Motor x) =
    case subMotor x of
    0 -> Scalar <| exAscalar id x
    1 -> Scalar <| exAscalar id x
    2 -> Line <| exBivectors id x
    4 -> Pseudo <| exApseudo id x
    otherwise -> m
contract f@(Flector x) =
    case subFlector x of
    8 -> Point <| exVectors id x
    16 -> Plane <| exTrivectors id x
    otherwise -> f
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
expand :: forall a. Ring a => GeoAlg a -> MultivecRec a ()
expand (Scalar x) = inAscalar const x mvzero
expand (Pseudo x) = inApseudo const x mvzero
expand (Point x) = inVectors const x mvzero
expand (Line x) = inBivectors const x mvzero
expand (Plane x) = inTrivectors const x mvzero
expand (Motor x) = inMotor const x mvzero
expand (Flector x) = inFlector const x mvzero
expand (GeoAlg x) = x

zeroG :: forall a. Semiring a => GeoAlg a
zeroG = Scalar zero

oneG :: forall a. Semiring a => GeoAlg a
oneG = Scalar one

addGeo :: forall a. Ring a => GeoAlg a -> GeoAlg a -> GeoAlg a
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
