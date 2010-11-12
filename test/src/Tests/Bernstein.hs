{-# LANGUAGE ExtendedDefaultRules #-}
module Tests.Bernstein (bernsteinTests) where

import Math.Polynomial
import Math.Polynomial.Bernstein
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import TestUtils

default (Integer, Rational)

bernsteinTests =
    [ testGroup "bernstein"     bernstein_tests
    , testGroup "evalBernstein" evalBernstein_tests
    ]

bernstein_tests =
    [ testProperty "sum"        prop_bernstein_sum
    , testProperty "recurrence" prop_bernstein_recurrence
    , testProperty "symmetry"   prop_bernstein_symmetry
    ]

-- prop_bernstein_sum can be very time and space intensive if we let it
-- look too far into the table.  We also don't really gain much if any 
-- additional confidence in the correctness of the implementation by doing
-- so, so here's a limit for 'n' in the "bernstein :: [[Poly Integer]]" tests.
bernsteinLimit = 500

prop_bernstein_sum (NonNegative n) =
    polyIsOne (sumPolys (bernstein !! (n `mod` bernsteinLimit)))

prop_bernstein_recurrence (Positive a) (Positive b) =
    let a' = a `mod` bernsteinLimit
        b' = b `mod` bernsteinLimit
        n = 1 + max a' b'; n' = fromIntegral n
        v =     min a' b'; v' = fromIntegral v
     in addPoly (scalePoly ((n'-v')/n') (fmap toRational (bernstein !! n !!  v   )))
                (scalePoly ((v'+ 1)/n') (fmap toRational (bernstein !! n !! (v+1))))
        == fmap toRational (bernstein !! (n-1) !! v)
            

prop_bernstein_symmetry (Positive a) (Positive b) =
    let a' = a `mod` bernsteinLimit
        b' = b `mod` bernsteinLimit
        n = max a' b'
        v = min a' b'
     in bernstein !! n !! (n-v)
     == composePoly (bernstein !! n !! v) (poly LE [1, -1])
            

evalBernstein_tests = 
    [ testProperty "sane"       prop_evalBernstein_sane
    , testProperty "sign"       prop_evalBernstein_sign
    , testProperty "local max"  prop_evalBernstein_local_max
    , testProperty "symmetry"   prop_evalBernstein_symmetry
    ]

prop_evalBernstein_sane (NonNegative a) (NonNegative b) x =
    let n = max a b
        v = min a b
     in n <= bernsteinLimit ==>
            evalPoly (fmap toRational (bernstein !! n !! v)) x
            == evalBernstein n v x

prop_evalBernstein_sign (NonNegative a) (NonNegative b) x =
    let n = max a b
        v = min a b
     in signum (evalBernstein n v x) == evalBernstein_sign n v x

prop_evalBernstein_local_max  (NonNegative a) (NonNegative b) x =
    let n = max a b
        v = min a b
        xMax = fromIntegral v / fromIntegral n
     in n >= 0 ==>
            evalBernstein n v (onUnitInterval x) <= evalBernstein n v xMax

prop_evalBernstein_symmetry  (NonNegative a) (NonNegative b) x =
    let n = max a b
        v = min a b
     in evalBernstein n (n-v) x
     == evalBernstein n    v  (1-x)

evalBernstein_sign 0 0 _ = 1
evalBernstein_sign n v 0 = delta v 0
evalBernstein_sign n v 1 = delta v n
evalBernstein_sign n v x
    | x > 0 && x < 1    = 1
    | even n || x < 0   = if even v then 1 else -1
    | otherwise         = if odd  v then 1 else -1 

delta x y
    | x == y    = 1
    | otherwise = 0