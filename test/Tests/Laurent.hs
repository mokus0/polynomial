{-# LANGUAGE ExtendedDefaultRules, TypeApplications, TypeSynonymInstances, FlexibleInstances ,OverlappingInstances #-}
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
default (Integer, Rational)

instance (Num a, Eq a, Arbitrary a) => Arbitrary (Laurent a) where
    arbitrary = newLaurent <$> arbitrary <*> arbitrary

instance Arbitrary Rational where
  arbitrary = fromInteger <$> arbitrary

laurentTests =
  [ testGroup "Laurent polynomials"
      [ testGroup "addLaurent"
        [ testProperty "left  unit"  $ \p     -> addLaurent @Rational zeroLaurent p == p
        , testProperty "right unit"  $ \p     -> addLaurent @Rational p zeroLaurent == p
        , testProperty "commutative" $ \p q   -> addLaurent @Rational p q == addLaurent q p
        , testProperty "associative" $ \p q r ->
            addLaurent @Rational p (addLaurent q r) == addLaurent (addLaurent p q) r
        , testProperty "sane" $ forAll (arbitrary `suchThat` (> 0)) $ \x -> \p q ->
                let e1 = evalLaurent @Rational x (addLaurent p q)
                    e2 = evalLaurent x p + evalLaurent x q
                in e1 == e2
        ]
      , testGroup "multLaurent"
        [ testProperty "left  cancel" $ \p     -> laurentIsZero (multLaurent @Rational zeroLaurent p)
        , testProperty "right cancel" $ \p     -> laurentIsZero (multLaurent @Rational p zeroLaurent)
        , testProperty "left  unit"   $ \p     -> multLaurent @Rational oneLaurent p == p
        , testProperty "right  unit"  $ \p     -> multLaurent @Rational p oneLaurent == p
        , testProperty "commutative"  $ \p q   -> multLaurent @Rational p q == multLaurent q p
        , testProperty "associative"  $ \p q r ->
            multLaurent @Rational p (multLaurent q r) == multLaurent (multLaurent p q) r
        , testProperty "distributive" $ \p q r ->
            (multLaurent @Rational p (addLaurent q r) == addLaurent (multLaurent p q) (multLaurent p r))
        , testProperty "sane" $ forAll (arbitrary `suchThat` (> 0)) $ \x -> \p q ->
            evalLaurent x (multLaurent @Rational p q)==
            evalLaurent x p * evalLaurent x q
        ]
      , testGroup "quotLaurent"
        [ testProperty "sane" $ \a b ->
            not (laurentIsZero b) ==>
              let (q, r) = quotRemLaurent @Rational a b
              in addLaurent (multLaurent q b) r == a
        ]
      ]
    ]

