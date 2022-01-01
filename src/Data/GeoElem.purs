module GeoElem where

import ZPrelude

import Data.Enum (class BoundedEnum, class Enum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericCardinality,
    genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep.Show (genericShow)


data Element = E | E1 | E2 | E3 | E4 |
    E23 | E31 | E12 | E43 | E42 | E41 |
    E321 | E124 | E314 | E234 | E1234

derive instance eqElement :: Eq Element
derive instance ordElement :: Ord Element
derive instance genericElement :: Generic Element _
instance showElement :: Show Element where
  show = genericShow
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
