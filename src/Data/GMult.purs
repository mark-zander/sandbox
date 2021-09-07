module GMult where

import ZPrelude
import Multivec

gp :: forall a. Ring a =>
    MultivecRec a () -> MultivecRec a () -> MultivecRec a ()
gp a b = 
 {e: a.e * b.e + a.e1 * b.e1 + a.e2 * b.e2 + a.e3 * b.e3 - a.e23 * b.e23 - a.e31 * b.e31 - a.e12 * b.e12 - a.e321 * b.e321,
e1: a.e * b.e1 + a.e1 * b.e - a.e2 * b.e12 + a.e3 * b.e31 + a.e23 * b.e321 - a.e31 * b.e3 + a.e12 * b.e2 + a.e321 * b.e23,
e2: a.e * b.e2 + a.e1 * b.e12 + a.e2 * b.e - a.e3 * b.e23 + a.e23 * b.e3 + a.e31 * b.e321 - a.e12 * b.e1 + a.e321 * b.e31,
e3: a.e * b.e3 - a.e1 * b.e31 + a.e2 * b.e23 + a.e3 * b.e - a.e23 * b.e2 + a.e31 * b.e1 + a.e12 * b.e321 + a.e321 * b.e12,
e4: a.e * b.e4 - a.e1 * b.e41 - a.e2 * b.e42 - a.e3 * b.e43 + a.e4 * b.e - a.e23 * b.e234 - a.e31 * b.e314 - a.e12 * b.e124 + a.e43 * b.e3 + a.e42 * b.e2 + a.e41 * b.e1 + a.e321 * b.e1234 - a.e124 * b.e12 - a.e314 * b.e31 - a.e234 * b.e23 - a.e1234 * b.e321,
e23: a.e * b.e23 - a.e1 * b.e321 + a.e2 * b.e3 - a.e3 * b.e2 + a.e23 * b.e - a.e31 * b.e12 + a.e12 * b.e31 - a.e321 * b.e1,
e31: a.e * b.e31 - a.e1 * b.e3 - a.e2 * b.e321 + a.e3 * b.e1 + a.e23 * b.e12 + a.e31 * b.e - a.e12 * b.e23 - a.e321 * b.e2,
e12: a.e * b.e12 + a.e1 * b.e2 - a.e2 * b.e1 - a.e3 * b.e321 - a.e23 * b.e31 + a.e31 * b.e23 + a.e12 * b.e - a.e321 * b.e3,
e43: a.e * b.e43 + a.e1 * b.e314 - a.e2 * b.e234 - a.e3 * b.e4 + a.e4 * b.e3 - a.e23 * b.e42 + a.e31 * b.e41 + a.e12 * b.e1234 + a.e43 * b.e + a.e42 * b.e23 - a.e41 * b.e31 - a.e321 * b.e124 + a.e124 * b.e321 + a.e314 * b.e1 - a.e234 * b.e2 + a.e1234 * b.e12,
e42: a.e * b.e42 - a.e1 * b.e124 - a.e2 * b.e4 + a.e3 * b.e234 + a.e4 * b.e2 + a.e23 * b.e43 + a.e31 * b.e1234 - a.e12 * b.e41 - a.e43 * b.e23 + a.e42 * b.e + a.e41 * b.e12 - a.e321 * b.e314 - a.e124 * b.e1 + a.e314 * b.e321 + a.e234 * b.e3 + a.e1234 * b.e31,
e41: a.e * b.e41 - a.e1 * b.e4 + a.e2 * b.e124 - a.e3 * b.e314 + a.e4 * b.e1 + a.e23 * b.e1234 - a.e31 * b.e43 + a.e12 * b.e42 + a.e43 * b.e31 - a.e42 * b.e12 + a.e41 * b.e - a.e321 * b.e234 + a.e124 * b.e2 - a.e314 * b.e3 + a.e234 * b.e321 + a.e1234 * b.e23,
e321: a.e * b.e321 - a.e1 * b.e23 - a.e2 * b.e31 - a.e3 * b.e12 - a.e23 * b.e1 - a.e31 * b.e2 - a.e12 * b.e3 + a.e321 * b.e,
e124: a.e * b.e124 - a.e1 * b.e42 + a.e2 * b.e41 + a.e3 * b.e1234 + a.e4 * b.e12 - a.e23 * b.e314 + a.e31 * b.e234 + a.e12 * b.e4 - a.e43 * b.e321 - a.e42 * b.e1 + a.e41 * b.e2 + a.e321 * b.e43 + a.e124 * b.e + a.e314 * b.e23 - a.e234 * b.e31 - a.e1234 * b.e3,
e314: a.e * b.e314 + a.e1 * b.e43 + a.e2 * b.e1234 - a.e3 * b.e41 + a.e4 * b.e31 + a.e23 * b.e124 + a.e31 * b.e4 - a.e12 * b.e234 + a.e43 * b.e1 - a.e42 * b.e321 - a.e41 * b.e3 + a.e321 * b.e42 - a.e124 * b.e23 + a.e314 * b.e + a.e234 * b.e12 - a.e1234 * b.e2,
e234: a.e * b.e234 + a.e1 * b.e1234 - a.e2 * b.e43 + a.e3 * b.e42 + a.e4 * b.e23 + a.e23 * b.e4 - a.e31 * b.e124 + a.e12 * b.e314 - a.e43 * b.e2 + a.e42 * b.e3 - a.e41 * b.e321 + a.e321 * b.e41 + a.e124 * b.e31 - a.e314 * b.e12 + a.e234 * b.e - a.e1234 * b.e1,
e1234: a.e * b.e1234 + a.e1 * b.e234 + a.e2 * b.e314 + a.e3 * b.e124 + a.e4 * b.e321 - a.e23 * b.e41 - a.e31 * b.e42 - a.e12 * b.e43 - a.e43 * b.e12 - a.e42 * b.e31 - a.e41 * b.e23 - a.e321 * b.e4 - a.e124 * b.e3 - a.e314 * b.e2 - a.e234 * b.e1 + a.e1234 * b.e
}

xp :: forall a. Ring a =>
    MultivecRec a () -> MultivecRec a () -> MultivecRec a ()
xp a b = 
 {e: zero,
e1: a.e * b.e1 + a.e1 * b.e,
e2: a.e * b.e2 + a.e2 * b.e,
e3: a.e * b.e3 + a.e3 * b.e,
e4: a.e * b.e4 + a.e4 * b.e,
e23: a.e * b.e23 + a.e2 * b.e3 - a.e3 * b.e2 + a.e23 * b.e,
e31: a.e * b.e31 - a.e1 * b.e3 + a.e3 * b.e1 + a.e31 * b.e,
e12: a.e * b.e12 + a.e1 * b.e2 - a.e2 * b.e1 + a.e12 * b.e,
e43: a.e * b.e43 - a.e3 * b.e4 + a.e4 * b.e3 + a.e43 * b.e,
e42: a.e * b.e42 - a.e2 * b.e4 + a.e4 * b.e2 + a.e42 * b.e,
e41: a.e * b.e41 - a.e1 * b.e4 + a.e4 * b.e1 + a.e41 * b.e,
e321: a.e * b.e321 - a.e1 * b.e23 - a.e2 * b.e31 - a.e3 * b.e12 - a.e23 * b.e1 - a.e31 * b.e2 - a.e12 * b.e3 + a.e321 * b.e,
e124: a.e * b.e124 - a.e1 * b.e42 + a.e2 * b.e41 + a.e4 * b.e12 + a.e12 * b.e4 - a.e42 * b.e1 + a.e41 * b.e2 + a.e124 * b.e,
e314: a.e * b.e314 + a.e1 * b.e43 - a.e3 * b.e41 + a.e4 * b.e31 + a.e31 * b.e4 + a.e43 * b.e1 - a.e41 * b.e3 + a.e314 * b.e,
e234: a.e * b.e234 - a.e2 * b.e43 + a.e3 * b.e42 + a.e4 * b.e23 + a.e23 * b.e4 - a.e43 * b.e2 + a.e42 * b.e3 + a.e234 * b.e,
e1234: a.e * b.e1234 + a.e1 * b.e234 + a.e2 * b.e314 + a.e3 * b.e124 + a.e4 * b.e321 - a.e23 * b.e41 - a.e31 * b.e42 - a.e12 * b.e43 - a.e43 * b.e12 - a.e42 * b.e31 - a.e41 * b.e23 - a.e321 * b.e4 - a.e124 * b.e3 - a.e314 * b.e2 - a.e234 * b.e1 + a.e1234 * b.e
}

gap :: forall a. Ring a =>
    MultivecRec a () -> MultivecRec a () -> MultivecRec a ()
gap a b = 
 {e: a.e * b.e1234 + a.e1 * b.e234 + a.e2 * b.e314 + a.e3 * b.e124 + a.e4 * b.e321 - a.e23 * b.e41 - a.e31 * b.e42 - a.e12 * b.e43 - a.e43 * b.e12 - a.e42 * b.e31 - a.e41 * b.e23 - a.e321 * b.e4 - a.e124 * b.e3 - a.e314 * b.e2 - a.e234 * b.e1 + a.e1234 * b.e,
e1: a.e * b.e234 + a.e1 * b.e1234 + a.e2 * b.e43 - a.e3 * b.e42 - a.e4 * b.e23 + a.e23 * b.e4 + a.e31 * b.e124 - a.e12 * b.e314 - a.e43 * b.e2 + a.e42 * b.e3 + a.e41 * b.e321 + a.e321 * b.e41 + a.e124 * b.e31 - a.e314 * b.e12 - a.e234 * b.e + a.e1234 * b.e1,
e2: a.e * b.e314 - a.e1 * b.e43 + a.e2 * b.e1234 + a.e3 * b.e41 - a.e4 * b.e31 - a.e23 * b.e124 + a.e31 * b.e4 + a.e12 * b.e234 + a.e43 * b.e1 + a.e42 * b.e321 - a.e41 * b.e3 + a.e321 * b.e42 - a.e124 * b.e23 - a.e314 * b.e + a.e234 * b.e12 + a.e1234 * b.e2,
e3: a.e * b.e124 + a.e1 * b.e42 - a.e2 * b.e41 + a.e3 * b.e1234 - a.e4 * b.e12 + a.e23 * b.e314 - a.e31 * b.e234 + a.e12 * b.e4 + a.e43 * b.e321 - a.e42 * b.e1 + a.e41 * b.e2 + a.e321 * b.e43 - a.e124 * b.e + a.e314 * b.e23 - a.e234 * b.e31 + a.e1234 * b.e3,
e4: a.e4 * b.e1234 - a.e43 * b.e124 - a.e42 * b.e314 - a.e41 * b.e234 - a.e124 * b.e43 - a.e314 * b.e42 - a.e234 * b.e41 + a.e1234 * b.e4,
e23: a.e * b.e41 - a.e1 * b.e4 - a.e2 * b.e124 + a.e3 * b.e314 + a.e4 * b.e1 + a.e23 * b.e1234 + a.e31 * b.e43 - a.e12 * b.e42 - a.e43 * b.e31 + a.e42 * b.e12 + a.e41 * b.e - a.e321 * b.e234 - a.e124 * b.e2 + a.e314 * b.e3 + a.e234 * b.e321 + a.e1234 * b.e23,
e31: a.e * b.e42 + a.e1 * b.e124 - a.e2 * b.e4 - a.e3 * b.e234 + a.e4 * b.e2 - a.e23 * b.e43 + a.e31 * b.e1234 + a.e12 * b.e41 + a.e43 * b.e23 + a.e42 * b.e - a.e41 * b.e12 - a.e321 * b.e314 + a.e124 * b.e1 + a.e314 * b.e321 - a.e234 * b.e3 + a.e1234 * b.e31,
e12: a.e * b.e43 - a.e1 * b.e314 + a.e2 * b.e234 - a.e3 * b.e4 + a.e4 * b.e3 + a.e23 * b.e42 - a.e31 * b.e41 + a.e12 * b.e1234 + a.e43 * b.e - a.e42 * b.e23 + a.e41 * b.e31 - a.e321 * b.e124 + a.e124 * b.e321 - a.e314 * b.e1 + a.e234 * b.e2 + a.e1234 * b.e12,
e43: negate a.e4 * b.e124 + a.e43 * b.e1234 - a.e42 * b.e41 + a.e41 * b.e42 - a.e124 * b.e4 + a.e314 * b.e234 - a.e234 * b.e314 + a.e1234 * b.e43,
e42: negate a.e4 * b.e314 + a.e43 * b.e41 + a.e42 * b.e1234 - a.e41 * b.e43 - a.e124 * b.e234 - a.e314 * b.e4 + a.e234 * b.e124 + a.e1234 * b.e42,
e41: negate a.e4 * b.e234 - a.e43 * b.e42 + a.e42 * b.e43 + a.e41 * b.e1234 + a.e124 * b.e314 - a.e314 * b.e124 - a.e234 * b.e4 + a.e1234 * b.e41,
e321: a.e * b.e4 - a.e1 * b.e41 - a.e2 * b.e42 - a.e3 * b.e43 - a.e4 * b.e - a.e23 * b.e234 - a.e31 * b.e314 - a.e12 * b.e124 - a.e43 * b.e3 - a.e42 * b.e2 - a.e41 * b.e1 + a.e321 * b.e1234 + a.e124 * b.e12 + a.e314 * b.e31 + a.e234 * b.e23 + a.e1234 * b.e321,
e124: a.e4 * b.e43 + a.e43 * b.e4 - a.e42 * b.e234 + a.e41 * b.e314 + a.e124 * b.e1234 - a.e314 * b.e41 + a.e234 * b.e42 + a.e1234 * b.e124,
e314: a.e4 * b.e42 + a.e43 * b.e234 + a.e42 * b.e4 - a.e41 * b.e124 + a.e124 * b.e41 + a.e314 * b.e1234 - a.e234 * b.e43 + a.e1234 * b.e314,
e234: a.e4 * b.e41 - a.e43 * b.e314 + a.e42 * b.e124 + a.e41 * b.e4 - a.e124 * b.e42 + a.e314 * b.e43 + a.e234 * b.e1234 + a.e1234 * b.e234,
e1234: negate a.e4 * b.e4 - a.e43 * b.e43 - a.e42 * b.e42 - a.e41 * b.e41 + a.e124 * b.e124 + a.e314 * b.e314 + a.e234 * b.e234 + a.e1234 * b.e1234
}

xap :: forall a. Ring a =>
    MultivecRec a () -> MultivecRec a () -> MultivecRec a ()
xap a b = 
 {e: a.e * b.e1234 + a.e1 * b.e234 + a.e2 * b.e314 + a.e3 * b.e124 + a.e4 * b.e321 - a.e23 * b.e41 - a.e31 * b.e42 - a.e12 * b.e43 - a.e43 * b.e12 - a.e42 * b.e31 - a.e41 * b.e23 - a.e321 * b.e4 - a.e124 * b.e3 - a.e314 * b.e2 - a.e234 * b.e1 + a.e1234 * b.e,
e1: a.e1 * b.e1234 + a.e31 * b.e124 - a.e12 * b.e314 + a.e41 * b.e321 + a.e321 * b.e41 + a.e124 * b.e31 - a.e314 * b.e12 + a.e1234 * b.e1,
e2: a.e2 * b.e1234 - a.e23 * b.e124 + a.e12 * b.e234 + a.e42 * b.e321 + a.e321 * b.e42 - a.e124 * b.e23 + a.e234 * b.e12 + a.e1234 * b.e2,
e3: a.e3 * b.e1234 + a.e23 * b.e314 - a.e31 * b.e234 + a.e43 * b.e321 + a.e321 * b.e43 + a.e314 * b.e23 - a.e234 * b.e31 + a.e1234 * b.e3,
e4: a.e4 * b.e1234 - a.e43 * b.e124 - a.e42 * b.e314 - a.e41 * b.e234 - a.e124 * b.e43 - a.e314 * b.e42 - a.e234 * b.e41 + a.e1234 * b.e4,
e23: a.e23 * b.e1234 - a.e321 * b.e234 + a.e234 * b.e321 + a.e1234 * b.e23,
e31: a.e31 * b.e1234 - a.e321 * b.e314 + a.e314 * b.e321 + a.e1234 * b.e31,
e12: a.e12 * b.e1234 - a.e321 * b.e124 + a.e124 * b.e321 + a.e1234 * b.e12,
e43: a.e43 * b.e1234 + a.e314 * b.e234 - a.e234 * b.e314 + a.e1234 * b.e43,
e42: a.e42 * b.e1234 - a.e124 * b.e234 + a.e234 * b.e124 + a.e1234 * b.e42,
e41: a.e41 * b.e1234 + a.e124 * b.e314 - a.e314 * b.e124 + a.e1234 * b.e41,
e321: a.e321 * b.e1234 + a.e1234 * b.e321,
e124: a.e124 * b.e1234 + a.e1234 * b.e124,
e314: a.e314 * b.e1234 + a.e1234 * b.e314,
e234: a.e234 * b.e1234 + a.e1234 * b.e234,
e1234: zero
}

