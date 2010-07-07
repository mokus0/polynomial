{-# LANGUAGE ExtendedDefaultRules #-}
module BernsteinTests where

import Math.Polynomial
import Math.Polynomial.Bernstein
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

default (Integer, Rational)

tests =
    [ testGroup "bernstein"
        [ testProperty "sum" $ \(NonNegative n) ->
            n <= 400 ==> polyIsOne (sumPolys (bernstein !! n))
        ]
    , testGroup "evalBernstein"
        [ testProperty "sane" $ \(NonNegative a) (NonNegative b) x ->
            let n = max a b
                v = min a b
             in n <= 1000 ==>
                    evalPoly (fmap fromInteger (bernstein !! n !! v)) x
                    == evalBernstein n v x
        ]
    ]
