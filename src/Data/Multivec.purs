-- | 16 Basis elements of the 4D projective geometric algebra
-- | broken into groups.
module Multivec where

import ZPrelude

-- import Prim.Row

import Data.Array (zipWith)
import Record (disjointUnion)
import Math (sqrt)
import Data.Foldable (sum)

-- Rows - basic elements of Projective GA

type AscalarRow a r = ( e :: a | r )

type BivectorsRow a r =
    ( e23 :: a  , e31 :: a  , e12 :: a
    , e43 :: a  , e42 :: a  , e41 :: a | r )

type VectorsRow a r =
    (e1 :: a   , e2 :: a   , e3 :: a   , e4 :: a | r)

type TrivectorsRow a r =
    (e321 :: a , e124 :: a , e314 :: a , e234 :: a | r)

type ApseudoRow a r = ( e1234 :: a | r )

data BkWt = Bulk | Weight

-- Rows - combining basic PGA elements

type MotorRow a r =
    AscalarRow a (BivectorsRow a (ApseudoRow a r))

type FlectorRow a r = VectorsRow a (TrivectorsRow a r)

-- Records - turn the rows into records
-- Multivec is a record but not a row, nothing else will
-- be made from it.

type AscalarRec a r = Record (AscalarRow a r)

type BivectorsRec a r = Record (BivectorsRow a r)

type ApseudoRec a r = Record (ApseudoRow a r)

type VectorsRec a r = Record (VectorsRow a r)

type TrivectorsRec a r = Record (TrivectorsRow a r)

type MotorRec a r = Record (MotorRow a r)

type FlectorRec a r = Record (FlectorRow a r)

type MultivecRec a r = Record (MotorRow a (FlectorRow a r))

-- motor = Ascalar U Bivectors U Apseudo
motorU :: forall a. AscalarRec a () -> BivectorsRec a () ->
    ApseudoRec a () -> MotorRec a ()
motorU s b p = disjointUnion s (disjointUnion b p)

-- flector = Vectors U Trivectors
flectorU :: forall a.
    VectorsRec a () -> TrivectorsRec a () -> FlectorRec a ()
flectorU v t = disjointUnion v t

-- multivec = Motor U Flector
multivecU :: forall a.
    MotorRec a () -> FlectorRec a () -> MultivecRec a ()
multivecU m f = disjointUnion m f

-- Show records
showAscalarRec ::
    forall a r. Show a => AscalarRec a r -> String
showAscalarRec x = " e: " <> show x.e

showApseudoRec ::
    forall a r. Show a => ApseudoRec a r -> String
showApseudoRec x = " e1234: " <> show x.e1234

showVectorsRec ::
    forall a r. Show a => VectorsRec a r -> String
showVectorsRec x =
    ", e1: " <> show x.e1 <>
    ", e2: " <> show x.e2 <>
    ", e3: " <> show x.e3 <>
    ", e3: " <> show x.e4

showBivectorsRec ::
    forall a r. Show a => BivectorsRec a r -> String
showBivectorsRec x =
        " e23: " <> show x.e23 <>
        ", e31: " <> show x.e31 <>
        ", e12: " <> show x.e12 <>
        ", e43: " <> show x.e43 <>
        ", e42: " <> show x.e42 <>
        ", e41: " <> show x.e41

showTrivectorsRec ::
    forall a r. Show a => TrivectorsRec a r -> String
showTrivectorsRec x =
        " e321: " <> show x.e321 <>
        ", e124: " <> show x.e124 <>
        ", e314: " <> show x.e314 <>
        ", e234: " <> show x.e234

-- Force the element order to be the same as in the
-- Cayley Tables.

showAscalar :: forall a r. Show a => AscalarRec a r -> String
showAscalar x = "{" <> showAscalarRec x <> "}"

showApseudo :: forall a r. Show a => ApseudoRec a r -> String
showApseudo x = "{" <> showApseudoRec x <> "}"

showVectors :: forall a r. Show a => VectorsRec a r -> String
showVectors x = "{" <> showVectorsRec x <> "}"

showTrivectors ::
    forall a r. Show a => TrivectorsRec a r -> String
showTrivectors x = "{" <> showTrivectorsRec x <> "}"

showBivectors ::
    forall a r. Show a => BivectorsRec a r -> String
showBivectors x = "{" <> showBivectorsRec x <> "}"

showMotorRec :: forall a r. Show a => MotorRec a r -> String
showMotorRec x = "{" <>
    showAscalarRec x <> ", " <>
    showBivectorsRec x <> ", " <>
    showApseudoRec x <> "}"

showFlectorRec :: forall a r. Show a => FlectorRec a r -> String
showFlectorRec x = "{" <>
    showVectorsRec x <> ", " <>
    showTrivectorsRec x <> "}"

showMultivec :: forall a r. Show a => MultivecRec a r -> String
showMultivec x = "{" <>
    showAscalarRec x <> ", " <>
    showVectorsRec x <> ", " <>
    showBivectorsRec x <> ", " <>
    showTrivectorsRec x <> ", " <>
    showApseudoRec x <> "}"

-- Combine values from 2 record arguments then insert into
-- second record argument.
inAscalar :: forall a b c r rr. (a -> b -> c) ->
    AscalarRec a rr -> AscalarRec b r -> AscalarRec c r
inAscalar f x y = y { e = f x.e y.e }
inApseudo :: forall a b c r rr. (a -> b -> c) ->
    ApseudoRec a rr -> ApseudoRec b r -> ApseudoRec c r
inApseudo f x y = y { e1234 = f x.e1234 y.e1234 }
inVectors :: forall a b c r rr. (a -> b -> c) ->
    VectorsRec a rr -> VectorsRec b r -> VectorsRec c r
inVectors f x y = y
    { e1 = f x.e1 y.e1, e2 = f x.e2 y.e2
    , e3 = f x.e3 y.e3, e4 = f x.e4 y.e4}
inBivectors :: forall a b c r rr. (a -> b -> c) ->
    BivectorsRec a rr -> BivectorsRec b r -> BivectorsRec c r
inBivectors f x y = y
    { e23 = f x.e23 y.e23, e31 = f x.e31 y.e31
    , e12 = f x.e12 y.e12, e43 = f x.e43 y.e43
    , e42 = f x.e42 y.e42, e41 = f x.e41 y.e41}
inTrivectors :: forall a b c r rr. (a -> b -> c) ->
    TrivectorsRec a rr -> TrivectorsRec b r -> TrivectorsRec c r
inTrivectors f x y = y
    { e321 = f x.e321 y.e321, e124 = f x.e124 y.e124
    , e314 = f x.e314 y.e314, e234 = f x.e234 y.e234}

inMotor :: forall a b c r rr. (a -> b -> c) ->
    MotorRec a rr -> MotorRec b r -> MotorRec c r
inMotor f x y =
    inAscalar f x <| inBivectors f x <| inApseudo f x y

inFlector :: forall a b c r rr. (a -> b -> c) ->
    FlectorRec a rr -> FlectorRec b r -> FlectorRec c r
inFlector f x y =
    inVectors f x <| inTrivectors f x y

inMultivec :: forall a b c r rr. (a -> b -> c) ->
    MultivecRec a rr -> MultivecRec b r -> MultivecRec c r
inMultivec f x y =
    inMotor f x <| inFlector f x y

-- Extract a scalar from a record and apply the funtion
exAscalar :: forall a b r. (a -> b) ->
    AscalarRec a r -> AscalarRec b ()
-- exAscalar f y = { e: f y.e }
exAscalar f (y::AscalarRec a r) = { e: f y.e }
exApseudo :: forall a b r. (a -> b) ->
    ApseudoRec a r -> ApseudoRec b ()
exApseudo f y = { e1234: f y.e1234 }
exVectors :: forall a b r. (a -> b) ->
    VectorsRec a r -> VectorsRec b ()
exVectors f y =
    { e1: f y.e1, e2: f y.e2, e3: f y.e3, e4: f y.e4}
exBivectors ::  forall a b r. (a -> b) ->
    BivectorsRec a r -> BivectorsRec b ()
exBivectors f y =
    { e23: f y.e23, e31: f y.e31, e12: f y.e12
    , e43: f y.e43, e42: f y.e42, e41: f y.e41}
exTrivectors ::  forall a b r. (a -> b) ->
    TrivectorsRec a r -> TrivectorsRec b ()
exTrivectors f y =
    { e321: f y.e321, e124: f y.e124
    , e314: f y.e314, e234: f y.e234}

exMotor :: forall a b r. (a -> b) ->
    MotorRec a r -> MotorRec b ()
exMotor f y =
    motorU (exAscalar f y) (exBivectors f y) (exApseudo f y)

exFlector :: forall a b r. (a -> b) ->
    FlectorRec a r -> FlectorRec b ()
exFlector f y = flectorU (exVectors f y) (exTrivectors f y)

exMultivec :: forall a b r. (a -> b) ->
    MultivecRec a r -> MultivecRec b ()
exMultivec f y = multivecU (exMotor f y) (exFlector f y)

mkArray :: forall a. MultivecRec a () -> Array a
mkArray x = [ x.e, x.e1, x.e2, x.e3, x.e4
    , x.e23, x.e31, x.e12, x.e43, x.e42, x.e41
    , x.e321, x.e124, x.e314, x.e234, x.e1234 ]

-- Create a record from argument(s)
mkAscalar :: forall a. a -> AscalarRec a ()
mkAscalar x = { e: x }

mkApseudo :: forall a. a -> ApseudoRec a ()
mkApseudo x = { e1234: x }

mkVectors :: forall a. a -> a -> a -> a -> VectorsRec a ()
mkVectors x1 x2 x3 x4 = { e1: x1, e2: x2, e3: x3, e4: x4 }

mkBivectors ::
    forall a. a -> a -> a -> a -> a -> a -> BivectorsRec a ()
mkBivectors x1 x2 x3 x4 x5 x6 =
    { e23: x1  , e31: x2  , e12: x3
    , e43: x4  , e42: x5  , e41: x6 }

mkTrivectors ::
    forall a. a -> a -> a -> a -> TrivectorsRec a ()
mkTrivectors x1 x2 x3 x4 =
    { e321: x1, e124: x2, e314: x3, e234: x4}

setAscalar :: forall a. a -> AscalarRec a ()
setAscalar x = mkAscalar x

setApseudo :: forall a. a -> ApseudoRec a ()
setApseudo x = mkApseudo x

setVectors :: forall a. a -> VectorsRec a ()
setVectors x = mkVectors x x x x

setBivectors :: forall a. a -> BivectorsRec a ()
setBivectors x = mkBivectors x x x x x x

setTrivectors :: forall a. a -> TrivectorsRec a ()
setTrivectors x = mkTrivectors x x x x

setMotor :: forall a. a -> MotorRec a ()
setMotor x = motorU (mkAscalar x) (setBivectors x) (mkApseudo x)

setFlector :: forall a. a -> FlectorRec a ()
setFlector x = flectorU (setVectors x) (setTrivectors x)

setMultivec :: forall a. a -> MultivecRec a ()
setMultivec x = multivecU (setMotor x) (setFlector x)

ascalar0 = zero :: forall a. Semiring a => AscalarRec a ()
apseudo0 = zero :: forall a. Semiring a => ApseudoRec a ()
vectors0 = zero :: forall a. Semiring a => VectorsRec a ()
bivectors0 = zero :: forall a. Semiring a => BivectorsRec a ()
trivectors0
    = zero :: forall a. Semiring a => TrivectorsRec a ()
motor0 = zero :: forall a. Semiring a => MotorRec a ()
flector0 = zero :: forall a. Semiring a => FlectorRec a ()
mvzero = zero :: forall a. Semiring a => MultivecRec a ()

-- find subcomponents of a motor
subMotor ::
    forall a r. Eq a => Semiring a => MotorRec a r -> Int
subMotor x = let
    is = if ascalar0 == exAscalar id x then 0 else 1
    ib = if bivectors0 == exBivectors id x then 0 else 2
    ip = if apseudo0 == exApseudo id x then 0 else 4
    in is + ib + ip

-- find subcomponents of a flector
subFlector ::
    forall a r. Eq a => Semiring a => FlectorRec a r -> Int
subFlector x = let
    iv = if vectors0 == exVectors id x then 0 else 8
    it = if trivectors0 == exTrivectors id x then 0 else 16
    in iv + it

-- find subcomponents of a multivec
subMultivec ::
    forall a r. Eq a => Semiring a => MultivecRec a r -> Int
subMultivec x = subMotor x + subFlector x

compAscalar :: forall a b r. (a -> b) ->
    AscalarRec a r -> ApseudoRec b ()
compAscalar f { e } = { e1234: f e }

compApseudo :: forall a b r. (a -> b) ->
    ApseudoRec a r -> AscalarRec b ()
compApseudo f { e1234 } = { e: f e1234 }

compBivectors :: forall a b r. (a -> b) ->
    BivectorsRec a r -> BivectorsRec b ()
compBivectors f { e23, e31, e12, e43, e42, e41 } =
    { e23: f e41, e31: f e42, e12: f e43
    , e43: f e12, e42: f e31, e41: f e23 }

compVectors :: forall a b r. (a -> b) ->
    VectorsRec a r -> TrivectorsRec b ()
compVectors f { e1, e2, e3, e4 } =
    { e321: f e4, e124: f e3, e314: f e2, e234: f e1 }

compTrivectors :: forall a b r. (a -> b) ->
    TrivectorsRec a r -> VectorsRec b ()
compTrivectors f { e321, e124, e314, e234 } =
    { e1: f e234, e2: f e314
    , e3: f e124, e4: f e321 }

compMotor :: forall a b r. (a -> b) ->
    MotorRec a r -> MotorRec b ()
compMotor f x = motorU (compApseudo f x)
    (compBivectors f x) (compAscalar f x)

compFlector :: forall a b r. (a -> b) ->
    FlectorRec a r -> FlectorRec b ()
compFlector f x =
    flectorU (compTrivectors f x) (compVectors f x)

compMultivec :: forall a b. (a -> b) ->
    MultivecRec a () -> MultivecRec b ()
compMultivec f x =
    multivecU (compMotor f x) (compFlector f x)


bwAscalar :: forall a r. Semiring a =>
    AscalarRec a r -> BkWt -> a
bwAscalar {e: x} = case _ of
    Bulk -> x
    Weight -> zero

bwApseudo :: forall a r. Semiring a =>
    ApseudoRec a r -> BkWt -> a
bwApseudo {e1234: x} = case _ of
    Bulk -> zero
    Weight -> x

bwBivectors :: forall a r. BivectorsRec a r -> BkWt -> Array a
bwBivectors
    {e41: vx, e42: vy, e43: vz, e23: mx, e31: my, e12: mz} =
    case _ of
        Bulk -> [mx, my, mz]
        Weight -> [vx, vy, vz]

bwVectors :: forall a r. VectorsRec a r -> BkWt -> Array a
bwVectors {e1: px, e2: py, e3: pz, e4: pw} = case _ of
    Bulk ->[px, py, pz]
    Weight -> [pw]

bwTrivectors ::
    forall a r. TrivectorsRec a r -> BkWt -> Array a
bwTrivectors {e234: fx, e314: fy, e124: fz, e321: fw}
    = case _ of
        Bulk -> [fw]
        Weight -> [fx, fy, fz]

bwMotor :: forall a r. MotorRec a r -> BkWt -> Array a
bwMotor
    { e41: rx, e42: ry, e43: rz, e1234: rw
    , e23: ux, e31: uy, e12: uz, e: uw} = case _ of
        Bulk -> [ux, uy, uz]
        Weight -> [rx, ry, rz]

bwFlector :: forall a r. FlectorRec a r -> BkWt -> Array a
bwFlector
    { e1: sx, e2: sy, e3: sz, e4: sw
    , e234: hx, e314: hy, e124: hz, e321: hw} = case _ of
        Bulk -> [sx, sy, sz, hw]
        Weight -> [hx, hy, hz, sw]

bwMultivec :: forall a r. MultivecRec a r -> BkWt -> Array a
bwMultivec x bw = bwMotor x bw <> bwFlector x bw


geoPropMv :: forall a. Semiring a => (BkWt -> Array a) -> a
geoPropMv f = sum <| zipWith (*) (f Bulk) (f Weight)

bwNormMv :: forall a. Semiring a =>
    (BkWt -> Array a) -> BkWt -> a
bwNormMv f bw = sum <| map sq <| f bw

sqrtNorm :: (BkWt -> Array Number) -> BkWt -> Number
sqrtNorm f bw = sqrt (bwNormMv f bw)

geoNormMv :: (BkWt -> Array Number) -> Number
geoNormMv f = sqrt <| bwNormMv f Bulk / bwNormMv f Weight
