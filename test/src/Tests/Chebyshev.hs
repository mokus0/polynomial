{-# LANGUAGE ExtendedDefaultRules #-}
module Tests.Chebyshev (chebyshevTests) where

import Math.Polynomial
import Math.Polynomial.Chebyshev
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import TestUtils

default (Integer, Rational)

chebyshevTests =
    [ testGroup "ts" ts_tests
    , testGroup "us" us_tests
    , testProperty "Pell's equation" prop_pell's_eqn
    , testProperty "t n == ts !! n" $ \(NonNegative a) -> let n = a `mod` 1000 in t n == ts !! n
    , testProperty "u n == us !! n" $ \(NonNegative a) -> let n = a `mod` 1000 in u n == us !! n
    , testGroup "evalT" evalT_tests
    , testGroup "evalU" evalU_tests
    ]

ts_tests =
    [ testProperty "recurrence" prop_ts_recurrence
    , testProperty "parity"     prop_ts_parity
    ]

prop_ts_recurrence (NonNegative 0) = ts !! 0 == one
prop_ts_recurrence (NonNegative 1) = ts !! 1 == x
prop_ts_recurrence (NonNegative a) =
    let n = 1 + a `mod` 1200
     in (multPoly (scalePoly 2 x) (ts !! n))
     == addPoly (ts !! (n-1)) (ts !! (n+1))

prop_ts_parity (NonNegative n)
    | even n    = composePoly t (negatePoly x) == t
    | otherwise = composePoly t (negatePoly x) == negatePoly t
        where t = ts !! (n `mod` 1200)

us_tests =
    [ testProperty "recurrence" prop_us_recurrence
    , testProperty "parity"     prop_us_parity
    ]

prop_us_recurrence (NonNegative 0) = us !! 0 == one
prop_us_recurrence (NonNegative 1) = us !! 1 == scalePoly 2 x
prop_us_recurrence (NonNegative a) =
    let n = 1 + a `mod` 1200
     in (multPoly (scalePoly 2 x) (us !! n))
     == addPoly (us !! (n-1)) (us !! (n+1))

prop_us_parity (NonNegative n)
    | even n    = composePoly u (negatePoly x) == u
    | otherwise = composePoly u (negatePoly x) == negatePoly u
        where u = us !! (n `mod` 1200)

prop_pell's_eqn (Positive a) =
    let n = 1 +  a `mod` 800
     in powPoly (ts !! n) 2
     == addPoly one (multPoly (poly BE [1,0,-1]) (powPoly (us !! (n-1)) 2))

evalT_tests =
    [ testProperty "sane"           prop_evalT_sane
    , testProperty "limits (Float)"  (prop_evalT_limits (1e-7  :: Float))
    , testProperty "limits (Double)" (prop_evalT_limits (1e-15 :: Double))
    , testProperty "endpoints"       prop_evalT_endpoints
    ]

prop_evalT_sane (NonNegative a) x =
    let n = a `mod` 1000
     in evalT n x == evalPoly (t n) x

prop_evalT_limits eps (NonNegative n) x = abs (evalT (n `mod` 5000) (onInterval (-1) 1 x)) <= 1 + eps

prop_evalT_endpoints (NonNegative a) True  =
    let n = a `mod` 5000
     in evalT n 1 == 1
prop_evalT_endpoints (NonNegative a) False =
    let n = a `mod` 5000
     in evalT n (-1) == if even n then 1 else -1

evalU_tests =
    [ testProperty "sane"           prop_evalU_sane
    ]

prop_evalU_sane (NonNegative a) x =
    let n = a `mod` 1000
     in evalT n x == evalPoly (t n) x

