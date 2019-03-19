{-# LANGUAGE ExtendedDefaultRules, TypeSynonymInstances, FlexibleInstances #-}
module Tests.Laurent (laurentTests) where

import Data.List
import Data.VectorSpace
import Math.Polynomial
import Math.Polynomial.Laurent
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck
import TestUtils

-- Use exact math everywhere; we're not testing stability or speed,
-- just mathematical soundness.
default (Rational)

laurentTests =
  [ testGroup "Laurent polynomials"
      [ testGroup "addLaurent"
        [ testProperty "left  unit"  $ \p     -> addLaurent zeroLaurent p == p
        , testProperty "right unit"  $ \p     -> addLaurent p zeroLaurent == p
        , testProperty "commutative" $ \p q   -> addLaurent p q == addLaurent q p
        , testProperty "associative" $ \p q r ->
            addLaurent p (addLaurent q r) == addLaurent (addLaurent p q) r
        , testProperty "sane" $ forAll (arbitrary `suchThat` (> 0)) $ \x -> \p q ->
                let e1 = evalLaurent (addLaurent p q) x
                    e2 = evalLaurent p x + evalLaurent q x
                in e1 == e2
        ]
      , testGroup "multLaurent"
        [ testProperty "left  cancel" $ \p     -> laurentIsZero (multLaurent zeroLaurent p)
        , testProperty "right cancel" $ \p     -> laurentIsZero (multLaurent p zeroLaurent)
        , testProperty "left  unit"   $ \p     -> multLaurent oneLaurent p == p
        , testProperty "right  unit"  $ \p     -> multLaurent p oneLaurent == p
        , testProperty "commutative"  $ \p q   -> multLaurent p q == multLaurent q p
        , testProperty "associative"  $ \p q r ->
            multLaurent p (multLaurent q r) == multLaurent (multLaurent p q) r
        , testProperty "distributive" $ \p q r ->
            (multLaurent p (addLaurent q r) == addLaurent (multLaurent p q) (multLaurent p r))
        , testProperty "sane" $ forAll (arbitrary `suchThat` (> 0)) $ \x -> \p q ->
            evalLaurent (multLaurent p q) x ==
            evalLaurent p x * evalLaurent q x
        ]
      , testGroup "quotLaurent"
        [ testProperty "sane" $ \a b ->
            not (laurentIsZero b) ==>
              let (q, r) = quotRemLaurent a b
              in addLaurent (multLaurent q b) r == a
        ]
      ]
    ]

