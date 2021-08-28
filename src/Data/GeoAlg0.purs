module GeoAlg0 where

import ZPrelude
import Data.Foldable
import Data.Array
import Data.Functor

-- Operators /\ \/ |- -| |= =| /.\ \./ /*\ \*/

-- wedge  (Point {e1: px, e2: py, e3: pz, e4: pw}) (Point {e1: qx, e2: qy, e3: qz, e4: qw}) =
--   Line { e41: (qx * pw - px * qw), e42: (qy * pw - py * qw), e43: (qz * pw - pz * qw),
--     e23: (py * qz - pz * qy), e31: (pz * qx - px * qz), e12: (px * qy - py * qx) }

-- MultiVec = Proscalar + Vectors + Bivectors
--      + Trivectors + Antiscalar
type MultiVec a =
    { e :: a
    , e1 :: a   , e2 :: a   , e3 :: a   , e4 :: a
    , e23 :: a  , e31 :: a  , e12 :: a
    , e43 :: a  , e42 :: a  , e41 :: a
    , e321 :: a , e124 :: a , e314 :: a , e234 :: a
    , e1234 :: a
    }

type Proscalar a = { e :: a }

type Vectors a =
    { e1 :: a   , e2 :: a   , e3 :: a   , e4 :: a }

type Bivectors a =
    { e23 :: a  , e31 :: a  , e12 :: a
    , e43 :: a  , e42 :: a  , e41 :: a }

type Trivectors a =
    { e321 :: a , e124 :: a , e314 :: a , e234 :: a }

type Antiscalar a = { e1234 :: a }

-- Motor = Scalar + Line + PseudoScalar
type MotorRec a =
    { e :: a
    , e23 :: a  , e31 :: a  , e12 :: a
    , e43 :: a  , e42 :: a  , e41 :: a
    , e1234 :: a
    }

-- Flector = Point + Plane
type FlectorRec a =
    { e1 :: a   , e2 :: a   , e3 :: a   , e4 :: a
    , e321 :: a , e124 :: a , e314 :: a , e234 :: a
    }

-- data GeoAlg a = GeoAlg (MultivecRec a) | Scalar (ScalarRec a ())
--     | Point (VectorsRec a ()) | Line (BivectorsRec a ())
--     | Plane (TrivectorsRec a ()) | Pseudo (PseudoRec a ())
--     | Motor (MotorRec a ()) | Flector (FlectorRec a ())

-- newtype DualGeo a = DualGeo (GeoAlg a)

data GeoAlg a = GeoAlg (MultiVec a) | Scalar (Proscalar a)
    | Point (Vectors a) | Line (Bivectors a)
    | Plane (Trivectors a) | Pseudo (Antiscalar a)
    | Motor (MotorRec a) | Flector (FlectorRec a)

newtype DualGeo a = DualGeo (GeoAlg a)

-- Force the element order to be the same as in code
instance showGeoAlg :: Show a => Show (GeoAlg a) where
    show (Scalar x) = "(Scalar " <> show x <> ")"
    show (Pseudo x) = "(Pseudo " <> show x <> ")"
    show (Point x) = "(Point " <> show x <> ")"
    show (Plane x) = "(Plane " <>
        "{ e321: " <> show x.e321 <>
        ", e124: " <> show x.e124 <>
        ", e314: " <> show x.e314 <>
        ", e234: " <> show x.e234 <> "})"
    show (Line x) = "(Line " <>
        "{ e23: " <> show x.e23 <>
        ", e31: " <> show x.e31 <>
        ", e12: " <> show x.e12 <>
        ", e43: " <> show x.e43 <>
        ", e42: " <> show x.e42 <>
        ", e41: " <> show x.e41 <> "})"
    show (GeoAlg x) = "(GeoAlg " <>
        "{ e: " <> show x.e <>
        ", e1: " <> show x.e1 <>
        ", e2: " <> show x.e2 <>
        ", e3: " <> show x.e3 <>
        ", e4: " <> show x.e4 <>
        ", e23: " <> show x.e23 <>
        ", e31: " <> show x.e31 <>
        ", e12: " <> show x.e12 <>
        ", e43: " <> show x.e43 <>
        ", e42: " <> show x.e42 <>
        ", e41: " <> show x.e41 <>
        ", e321: " <> show x.e321 <>
        ", e124: " <> show x.e124 <>
        ", e314: " <> show x.e314 <>
        ", e234: " <> show x.e234 <>
        ", e1234: " <> show x.e1234 <> "})"
    show (Motor x) = "(GeoAlg " <>
        "{ e: " <> show x.e <>
        ", e23: " <> show x.e23 <>
        ", e31: " <> show x.e31 <>
        ", e12: " <> show x.e12 <>
        ", e43: " <> show x.e43 <>
        ", e42: " <> show x.e42 <>
        ", e41: " <> show x.e41 <>
        ", e1234: " <> show x.e1234 <> "})"
    show (Flector x) = "(GeoAlg " <>
        "{ e1: " <> show x.e1 <>
        ", e2: " <> show x.e2 <>
        ", e3: " <> show x.e3 <>
        ", e4: " <> show x.e4 <>
        ", e321: " <> show x.e321 <>
        ", e124: " <> show x.e124 <>
        ", e314: " <> show x.e314 <>
        ", e234: " <> show x.e234 <> "})"

setScalar :: forall a. a -> Proscalar a
setScalar x = { e: x }
setPseudo :: forall a. a -> Antiscalar a
setPseudo x = { e1234: x }
setPoint :: forall a. a -> Vectors a
setPoint x = { e1: x   , e2: x   , e3: x   , e4: x }
setLine :: forall a. a -> Bivectors a
setLine x =
    { e23: x  , e31: x  , e12: x, e43: x  , e42: x  , e41: x}
setPlane :: forall a. a -> Trivectors a
setPlane x = { e321: x , e124: x , e314: x , e234: x }
setMotor :: forall a. a -> MotorRec a
setMotor x =
    { e: x, e23: x  , e31: x  , e12: x
    , e43: x  , e42: x  , e41: x, e1234: x }
setFlector :: forall a. a -> FlectorRec a
setFlector x =
    { e1: x   , e2: x   , e3: x   , e4: x
    , e321: x , e124: x , e314: x , e234: x }
setGeoAlg :: forall a. a -> MultiVec a
setGeoAlg x =
    { e: x
    , e1: x   , e2: x   , e3: x   , e4: x
    , e23: x  , e31: x  , e12: x
    , e43: x  , e42: x  , e41: x
    , e321: x , e124: x , e314: x , e234: x
    , e1234: x
    }

mvzero :: forall a. Ring a => MultiVec a
mvzero = setGeoAlg zero

-- Empty mask full of false
falseMask :: MultiVec Boolean
falseMask = setGeoAlg false

trueMask :: MultiVec Boolean
trueMask = setGeoAlg true

-- Insert a subset into a full MultiVec
inScalar ::
    forall a. MultiVec a -> Proscalar a -> MultiVec a
inScalar x y = x { e = y.e }
inPseudo ::
    forall a. MultiVec a -> Antiscalar a -> MultiVec a
inPseudo x y = x { e1234 = y.e1234 }
inPoint ::
    forall a. MultiVec a -> Vectors a -> MultiVec a
inPoint x y = x
    { e1 = y.e1, e2 = y.e2, e3 = y.e3, e4 = y.e4}
inLine ::
    forall a. MultiVec a -> Bivectors a -> MultiVec a
inLine x y = x
    { e23 = y.e23, e31 = y.e31, e12 = y.e12
    , e43 = y.e43, e42 = y.e42, e41 = y.e41}
inPlane ::
    forall a. MultiVec a -> Trivectors a -> MultiVec a
inPlane x y = x
    { e321 = y.e321, e124 = y.e124, e314 = y.e314, e234 = y.e234}
inMotor ::
    forall a. MultiVec a -> MotorRec a -> MultiVec a
inMotor x y = x
    { e = y.e, e23 = y.e23, e31 = y.e31, e12 = y.e12
    , e43 = y.e43, e42 = y.e42, e41 = y.e41, e1234 = y.e1234 }
inFlector ::
    forall a. MultiVec a -> FlectorRec a -> MultiVec a
inFlector x y = x
    { e1 = y.e1, e2 = y.e2, e3 = y.e3, e4 = y.e4
    , e321 = y.e321, e124 = y.e124, e314 = y.e314, e234 = y.e234}


-- Extract from a MultiVec to a subset
exScalar :: forall a. MultiVec a -> Proscalar a
exScalar y = { e: y.e }
exPseudo :: forall a. MultiVec a -> Antiscalar a
exPseudo y = { e1234: y.e1234 }
exPoint :: forall a. MultiVec a -> Vectors a
exPoint y = { e1: y.e1, e2: y.e2, e3: y.e3, e4: y.e4}
exLine :: forall a. MultiVec a -> Bivectors a
exLine y =
    { e23: y.e23, e31: y.e31, e12: y.e12
    , e43: y.e43, e42: y.e42, e41: y.e41}
exPlane :: forall a. MultiVec a -> Trivectors a
exPlane y =
    { e321: y.e321, e124: y.e124, e314: y.e314, e234: y.e234}
exMotor :: forall a. MultiVec a -> MotorRec a
exMotor y =
    { e: y.e, e23: y.e23, e31: y.e31, e12: y.e12
    , e43: y.e43, e42: y.e42, e41: y.e41, e1234: y.e1234 }
exFlector :: forall a. MultiVec a -> FlectorRec a
exFlector y =
    { e1: y.e1, e2: y.e2, e3: y.e3, e4: y.e4
    , e321: y.e321, e124: y.e124, e314: y.e314, e234: y.e234}

-- scalarMask = mkArray <| inScalar falseMask <| setScalar true

scalarMask :: Array Boolean
scalarMask = mkArray <| inScalar trueMask <| setScalar false
pseudoMask :: Array Boolean
pseudoMask = mkArray <| inPseudo trueMask <| setPseudo false
pointMask :: Array Boolean
pointMask = mkArray <| inPoint trueMask <| setPoint false
lineMask :: Array Boolean
lineMask = mkArray <| inLine trueMask <| setLine false
planeMask :: Array Boolean
planeMask = mkArray <| inPlane trueMask <| setPlane false
motorMask :: Array Boolean
motorMask = mkArray <| inMotor trueMask <| setMotor false
flectorMask :: Array Boolean
flectorMask = mkArray <| inFlector trueMask <| setFlector false

mkArray :: forall a. MultiVec a -> Array a
mkArray x = [ x.e, x.e1, x.e2, x.e3, x.e4
    , x.e23, x.e31, x.e12, x.e43, x.e42, x.e41
    , x.e321, x.e124, x.e314, x.e234, x.e1234 ]

maskBy :: forall a. Ring a => Eq a =>
    Array Boolean -> MultiVec a -> Boolean
maskBy mask mv = all (_ == false) <| zipWith conj mask
     <| map (_ /= zero) <| mkArray mv

shrinkScalar :: forall a. Ring a => Eq a => GeoAlg a -> GeoAlg a
shrinkScalar y@(GeoAlg x) =
    if maskBy scalarMask x then Scalar <| exScalar x else y
shrinkScalar y@(Motor xx) = let x = inMotor mvzero xx in
    if maskBy scalarMask x then Scalar <| exScalar x else y
shrinkScalar y = y

shrinkPseudo :: forall a. Ring a => Eq a => GeoAlg a -> GeoAlg a
shrinkPseudo y@(GeoAlg x) =
    if maskBy pseudoMask x then Pseudo <| exPseudo x else y
shrinkPseudo y@(Motor xx) = let x = inMotor mvzero xx in
    if maskBy pseudoMask x then Pseudo <| exPseudo x else y
shrinkPseudo y = y

shrinkPoint :: forall a. Ring a => Eq a => GeoAlg a -> GeoAlg a
shrinkPoint y@(GeoAlg x) =
    if maskBy pointMask x then Point <| exPoint x else y
shrinkPoint y@(Flector xx) = let x = inFlector mvzero xx in
    if maskBy pointMask x then Point <| exPoint x else y
shrinkPoint y = y

shrinkLine :: forall a. Ring a => Eq a => GeoAlg a -> GeoAlg a
shrinkLine y@(GeoAlg x) =
    if maskBy lineMask x then Line <| exLine x else y
shrinkLine y@(Motor xx) = let x = inMotor mvzero xx in
    if maskBy lineMask x then Line <| exLine x else y
shrinkLine y = y

shrinkPlane :: forall a. Ring a => Eq a => GeoAlg a -> GeoAlg a
shrinkPlane y@(GeoAlg x) =
    if maskBy planeMask x then Plane <| exPlane x else y
shrinkPlane y@(Flector xx) = let x = inFlector mvzero xx in
    if maskBy planeMask x then Plane <| exPlane x else y
shrinkPlane y = y

shrinkMotor :: forall a. Ring a => Eq a => GeoAlg a -> GeoAlg a
shrinkMotor y@(GeoAlg x) =
    if maskBy motorMask x then Motor <| exMotor x else y
shrinkMotor y = y

shrinkFlector :: forall a. Ring a => Eq a => GeoAlg a -> GeoAlg a
shrinkFlector y@(GeoAlg x) =
    if maskBy flectorMask x then Flector <| exFlector x else y
shrinkFlector y = y

-- expand a subvector to a full Multivector
expand :: forall a. Ring a => GeoAlg a -> MultiVec a
expand (Scalar x) = inScalar mvzero x
expand (Pseudo x) = inPseudo mvzero x
expand (Point x) = inPoint mvzero x
expand (Line x) = inLine mvzero x
expand (Plane x) = inPlane mvzero x
expand (Motor x) = inMotor mvzero x
expand (Flector x) = inFlector mvzero x
expand (GeoAlg x) = x

-- Motorize a scalar, convert a scalar to a motor
motoSc :: forall a. Ring a => Proscalar a -> MotorRec a
motoSc x = exMotor <| inScalar mvzero x
-- Motorize a pseudoscalar
motoPs :: forall a. Ring a => Antiscalar a -> MotorRec a
motoPs x = exMotor <| inPseudo mvzero x
-- Motorize a line
motoLi :: forall a. Ring a => Bivectors a -> MotorRec a
motoLi x = exMotor <| inLine mvzero x

-- Flectorize a point
flecPt :: forall a. Ring a => Vectors a -> FlectorRec a
flecPt x = exFlector <| inPoint mvzero x
-- Flectorize a plane
flecPl :: forall a. Ring a => Trivectors a -> FlectorRec a
flecPl x = exFlector <| inPlane mvzero x

eqGeo :: forall a. Eq a => Ring a => GeoAlg a -> GeoAlg a -> Boolean
eqGeo (Scalar x) (Scalar y) = x == y
eqGeo (Pseudo x) (Pseudo y) = x == y
eqGeo (Point x) (Point y) = x == y
eqGeo (Line x) (Line y) = x == y
eqGeo (Plane x) (Plane y) = x == y
eqGeo (Motor x) (Motor y) = x == y
eqGeo (Flector x) (Flector y) = x == y
eqGeo (GeoAlg x) (GeoAlg y) = x == y
eqGeo x y = expand x == expand y

addGeo :: forall a. Ring a => GeoAlg a -> GeoAlg a -> GeoAlg a
addGeo (Scalar x) (Scalar y) = Scalar <| x + y
addGeo (Pseudo x) (Pseudo y) = Pseudo <| x + y
addGeo (Point x) (Point y) = Point <| x + y
addGeo (Line x) (Line y) = Line <| x + y
addGeo (Plane x) (Plane y) = Plane <| x + y
addGeo (Motor x) (Motor y) = Motor <| x + y
addGeo (Flector x) (Flector y) = Flector <| x + y
addGeo (GeoAlg x) (GeoAlg y) = GeoAlg <| x + y

addGeo (Scalar x) (Pseudo y) = Motor <| motoSc x + motoPs y
addGeo (Scalar x) (Line y) = Motor <| motoSc x + motoLi y
addGeo (Scalar x) (Motor y) = Motor <| motoSc x + y

addGeo (Pseudo x) (Scalar y) = Motor <| motoPs x + motoSc y
addGeo (Pseudo x) (Line y) = Motor <| motoPs x + motoLi y
addGeo (Pseudo x) (Motor y) = Motor <| motoPs x + y

addGeo (Line x) (Scalar y) = Motor <| motoLi x + motoSc y
addGeo (Line x) (Pseudo y) = Motor <| motoLi x + motoPs y
addGeo (Line x) (Motor y) = Motor <| motoLi x + y

addGeo (Motor x) (Scalar y) = Motor <| x + motoSc y
addGeo (Motor x) (Pseudo y) = Motor <| x + motoPs y
addGeo (Motor x) (Line y) = Motor <| x + motoLi y

addGeo (Point x) (Plane y) = Flector <| flecPt x + flecPl y
addGeo (Point x) (Flector y) = Flector <| flecPt x + y

addGeo (Plane x) (Point y) = Flector <| flecPl x + flecPt y
addGeo (Plane x) (Flector y) = Flector <| flecPl x + y

addGeo (Flector x) (Point y) = Flector <| x + flecPt y
addGeo (Flector x) (Plane y) = Flector <| x + flecPl y

addGeo x y = GeoAlg <| expand x + expand y

-- addGeo (Point x) yy = let y = expand yy in GeoAlg <| y
--     { e1 = x.e1 + y.e1, e2 = x.e2 + y.e2
--     , e3 = x.e3 + y.e3, e4 = x.e4 + y.e4 }

-- addGeo (Plane x) (Plane y) = Plane <| x + y
--     -- { e321: x.e321 + y.e321, e124: x.e124 + y.e124
--     -- , e314: x.e314 + y.e314, e234: x.e234 + y.e234 }
-- addGeo (Plane x) yy = let y = expand yy in GeoAlg <| y
--     { e321 = x.e321 + y.e321, e124 = x.e124 + y.e124
--     , e314 = x.e314 + y.e314, e234 = x.e234 + y.e234 }
-- addGeo (Flector x) (Flector y) = Flector <| x + y
-- addGeo x y = GeoAlg <| expand x + expand y

-- addGeo :: forall a. Ring a => GeoAlg a -> GeoAlg a -> GeoAlg a
-- addGeo (Scalar x) (Scalar y) = Scalar <| x + y
-- addGeo (Scalar x) yy = let y = expand yy in
--     GeoAlg <| y { e = x.e + y.e}
-- addGeo (Pseudo x) (Pseudo y) = Pseudo <| x + y
--     -- (Pseudo { e1234: x.e1234 + y.e1234})
-- addGeo (Pseudo x) yy = let y = expand yy in
--     GeoAlg <| y { e1234 = x.e1234 + y.e1234}
-- addGeo (Point x) (Point y) = Point <| x + y
--     -- { e1: x.e1 + y.e1, e2: x.e2 + y.e2
--     -- , e3: x.e3 + y.e3, e4: x.e4 + y.e4 }
-- addGeo (Point x) yy = let y = expand yy in GeoAlg <| y
--     { e1 = x.e1 + y.e1, e2 = x.e2 + y.e2
--     , e3 = x.e3 + y.e3, e4 = x.e4 + y.e4 }
-- addGeo (Line x) (Line y) = Line <| x + y
--     -- { e23: x.e23 + y.e23, e31: x.e31 + y.e31
--     -- , e12: x.e12 + y.e12, e43: x.e43 + y.e43
--     -- , e42: x.e42 + y.e42, e41: x.e41 + y.e41 }
-- addGeo (Line x) yy = let y = expand yy in GeoAlg <| y
--     { e23 = x.e23 + y.e23, e31 = x.e31 + y.e31
--     , e12 = x.e12 + y.e12, e43 = x.e43 + y.e43
--     , e42 = x.e42 + y.e42, e41 = x.e41 + y.e41 }
-- addGeo (Plane x) (Plane y) = Plane <| x + y
--     -- { e321: x.e321 + y.e321, e124: x.e124 + y.e124
--     -- , e314: x.e314 + y.e314, e234: x.e234 + y.e234 }
-- addGeo (Plane x) yy = let y = expand yy in GeoAlg <| y
--     { e321 = x.e321 + y.e321, e124 = x.e124 + y.e124
--     , e314 = x.e314 + y.e314, e234 = x.e234 + y.e234 }
-- addGeo (Motor x) (Motor y) = Motor <| x + y
-- addGeo (Flector x) (Flector y) = Flector <| x + y
-- addGeo (GeoAlg x) yy = GeoAlg <| x + expand yy


-- addMultiVec (GeoAlg x) yy = let y = expand yy in
--     GeoAlg { e: x.e + y.e
--     , e1: x.e1 + y.e1
--     , e2: x.e2 + y.e2
--     , e3: x.e3 + y.e3
--     , e4: x.e4 + y.e4
--     , e23: x.e23 + y.e23
--     , e31: x.e31 + y.e31
--     , e12: x.e12 + y.e12
--     , e43: x.e43 + y.e43
--     , e42: x.e42 + y.e42
--     , e41: x.e41 + y.e41
--     , e321: x.e321 + y.e321
--     , e124: x.e124 + y.e124
--     , e314: x.e314 + y.e314
--     , e234: x.e234 + y.e234
--     , e1234: x.e1234 + y.e1234
--     }


    -- | Vector
    -- { e1 :: a
    -- , e2 :: a
    -- , e3 :: a
    -- , e4 :: a
    -- }
    -- | Bivector
    -- { e23 :: a
    -- , e31 :: a
    -- , e12 :: a
    -- , e43 :: a
    -- , e42 :: a
    -- , e41 :: a
    -- }
    -- | TriVector
    -- { e321 :: a
    -- , e124 :: a
    -- , e314 :: a
    -- , e234 :: a
    -- }
    -- | PScalar { e1234 :: a }
    -- | Motor
    -- { e :: a
    -- , e23 :: a
    -- , e31 :: a
    -- , e12 :: a
    -- , e43 :: a
    -- , e42 :: a
    -- , e41 :: a
    -- , e1234 :: a
    -- }
    -- | Flector
    -- { e1 :: a
    -- , e2 :: a
    -- , e3 :: a
    -- , e4 :: a
    -- , e321 :: a
    -- , e124 :: a
    -- , e314 :: a
    -- , e234 :: a
    -- }

-- instance semiRingMultiVec :: Ring a => Semiring (MultiVector a) where
--   zero = Scalar { e: zero }
--   one = Scalar { e: one }
--   add mv1 mv2 = addMultiVec mv1 mv2
--   mul mv1 mv2 = mulMultiVec mv1 mv2

-- instance ringMultiVec :: Ring a => Ring (MultiVector a) where
--   sub mv1 mv2 = subMultiVec mv1 mv2

-- instance divisionRingMultiVec :: DivisionRing a => DivisionRing (MultiVector a) where
--   recip mv = recipMultiVec mv

-- addMultiVec :: forall a. MultiVector a -> MultiVector a -> MultiVector a
-- addMultiVec (Scalar x) (Scalar y) = Scalar { e: x.e + y.e }
-- addMultiVec _ _ = Scalar { e: 0 }

-- instance showMultiVec :: Show a => Show (MultiVec a) where
--   show x = "{ e:" <> x.e <> ", e1: " <> x.e1 <>
--     ", e2: " <> x.e2 <> "}"
