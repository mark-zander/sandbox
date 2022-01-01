-- Experimental Geometric Algebra
-- With simplified add and subtract
-- How to do multiply?
module GeoAlgX where

import ZPrelude

import GeoElem (Element(..))
import ArrayIx
import Control.Monad.ST as ST
import Data.Array.ST as STA
import Data.Enum
import Data.Maybe
import Data.Traversable
import Data.Tuple
import TablesX

import Partial.Unsafe (unsafePartial)

-- Operators /\ \/ |- -| |= =| /.\ \./ /*\ \*/

-- infix 7 geoProd as /.\
-- infix 7 geoAnti as \./
-- infix 7 extProd as /\
-- infix 7 extAnti as \/

-- Should this be a newtype?
type GeoAlg a = ArrayIx Element a

instance semiringGeoAlg :: Ring a => Semiring (GeoAlg a) where
  add xs ys = zipWithIx add xs ys
  zero = repeat zero
  mul = antiMult
  one = updateAt E1234 one zero

instance ringGeoAlg :: Ring a => Ring (GeoAlg a) where
  sub xs ys = zipWithIx sub xs ys

ixixTbl :: Array (Array TblEntry) -> GeoAlg (GeoAlg TblEntry)
ixixTbl xss = unsafePartial <| fromJust <| mkArrayIxArrayIx xss

ixTbl :: forall a. Array a -> GeoAlg a
ixTbl xss = unsafePartial <| fromJust <| mkArrayIx xss

gpTblIx = ixixTbl gpTbl :: GeoAlg (GeoAlg TblEntry)

gapTblIx = ixixTbl gapTbl :: GeoAlg (GeoAlg TblEntry)

grdTblIx = ixTbl grdTbl :: GeoAlg Int

cmplRTblIx = ixTbl cmplRTbl :: GeoAlg TblEntry

cmplLTblIx = ixTbl cmplRTbl :: GeoAlg TblEntry

cmplDTblIx = ixTbl cmplDTbl :: GeoAlg TblEntry

revTblIx = ixTbl revTbl :: GeoAlg TblEntry

revATblIx = ixTbl revATbl :: GeoAlg TblEntry

dotTblIx = ixTbl dotTbl :: GeoAlg TblEntry

dotATblIx = ixTbl dotATbl :: GeoAlg TblEntry

blkTblIx = ixTbl blkTbl :: GeoAlg Int

wghtTblIx = ixTbl wghtTbl :: GeoAlg Int


zerog :: forall a. Semiring a => GeoAlg a
zerog = repeat zero

oneg :: forall a. Semiring a => GeoAlg a
oneg = updateAt E1234 one zerog

addg :: forall a. Semiring a => GeoAlg a -> GeoAlg a -> GeoAlg a
addg xs ys = (+) <$> xs <*> ys

subg :: forall a. Ring a => GeoAlg a -> GeoAlg a -> GeoAlg a
subg xs ys = (-) <$> xs <*> ys

elements ∷ ArrayIx Element Element
elements = bottomToTop

gMultiply :: forall a. Ring a
  => GeoAlg (GeoAlg TblEntry)
  -> (TblEntry -> a -> a -> Tuple Element (a -> a))
  -> GeoAlg a
  -> GeoAlg a
  -> GeoAlg a
gMultiply tbl interp a b =
  unsafePartial <| fromJust <| mkArrayIx <|
    ST.run (flip STA.withArray (toArray zerog) (\res ->
      for_ elements (\i ->
        let
          tbli = tbl !! i
          ai = a !! i
        in
          for_ elements (\j ->
            let
              (Tuple ix f) = interp (tbli !! j) ai (b !! j)
            in STA.modify (fromEnum ix) f res
          )
      )
    ))

interpExt :: forall a. Ring a =>
  TblEntry -> a -> a -> Tuple Element (a -> a)
interpExt entry ai bj =
  case entry of
  XP e -> Tuple e (add (ai * bj))
  XN e -> Tuple e (minus (ai * bj))
  _ -> Tuple E id

interpGeo :: forall a. Ring a =>
  TblEntry -> a -> a -> Tuple Element (a -> a)
interpGeo entry ai bj =
  case entry of
  Z -> Tuple E id
  P e -> Tuple e (add (ai * bj))
  N e -> Tuple e (minus (ai * bj))
  XP e -> Tuple e (add (ai * bj))
  XN e -> Tuple e (minus (ai * bj))
 
geoMult :: forall a. Ring a => GeoAlg a -> GeoAlg a -> GeoAlg a
geoMult = gMultiply gpTblIx interpGeo

infixl 7 geoMult as /.\

antiMult :: forall a. Ring a => GeoAlg a -> GeoAlg a -> GeoAlg a
antiMult = gMultiply gapTblIx interpGeo

infixl 7 antiMult as \./

geoExt :: forall a. Ring a => GeoAlg a -> GeoAlg a -> GeoAlg a
geoExt = gMultiply gpTblIx interpExt

infixl 7 geoExt as /\

antiExt :: forall a. Ring a => GeoAlg a -> GeoAlg a -> GeoAlg a
antiExt = gMultiply gapTblIx interpExt

infixl 7 antiMult as \/

intrMultL :: forall a. Ring a => GeoAlg a -> GeoAlg a -> GeoAlg a
intrMultL a b = gMultiply gpTblIx interpExt (cmplL a) b

infixl 7 intrMultL as -|

intrMultR :: forall a. Ring a => GeoAlg a -> GeoAlg a -> GeoAlg a
intrMultR a b = gMultiply gpTblIx interpExt a (cmplR b)

infixl 7 intrMultR as |-

antiIntrL :: forall a. Ring a => GeoAlg a -> GeoAlg a -> GeoAlg a
antiIntrL a b = gMultiply gapTblIx interpExt (cmplL a) b

infixl 7 antiIntrL as =|

antiIntrR :: forall a. Ring a => GeoAlg a -> GeoAlg a -> GeoAlg a
antiIntrR a b = gMultiply gapTblIx interpExt a (cmplR b)

infixl 7 antiIntrR as |=

cmplL :: forall a. Ring a => GeoAlg a -> GeoAlg a
cmplL xs = gUniOp cmplLTblIx xs

cmplR :: forall a. Ring a => GeoAlg a -> GeoAlg a
cmplR xs = gUniOp cmplRTblIx xs

-- Use array instead of ArrayIx for 
-- for_ 
gUniOp :: forall a. Ring a
  => GeoAlg TblEntry
  -> GeoAlg a
  -> GeoAlg a
gUniOp tbl xs =
  unsafePartial <| fromJust <| mkArrayIx <|
    ST.run (flip STA.withArray (toArray zerog) (\res ->
        for_ elements (\i ->
          let
            x = xs !! i
            (Tuple ix f) = case tbl !! i of
              P e -> Tuple e (add x)
              N e -> Tuple e (minus x)
              _ -> Tuple E id
          in STA.modify (fromEnum ix) f res
        )
    ))

