{-# LANGUAGE ExtendedDefaultRules #-}
module BernsteinTests where

import Math.Polynomial
import Math.Polynomial.Bernstein
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

default (Integer, Rational)

tests =
    [ testGroup "bernstein"     bernstein_tests
    , testGroup "evalBernstein" evalBernstein_tests
    ]

bernstein_tests =
    [ testProperty "sum" prop_bernstein_sum
    ]

prop_bernstein_sum (NonNegative n) =
    n <= 400 ==> 
        polyIsOne (sumPolys (bernstein !! n))

evalBernstein_tests = 
    [ testProperty "sane" prop_evalBernstein_sane
    ]

prop_evalBernstein_sane (NonNegative a) (NonNegative b) x =
    let n = max a b
        v = min a b
     in n <= 600 ==>
            evalPoly (fmap toRational (bernstein !! n !! v)) x
            == evalBernstein n v x
