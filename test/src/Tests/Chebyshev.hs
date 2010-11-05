module Tests.Chebyshev (chebyshevTests) where

import Math.Polynomial
import Math.Polynomial.Chebyshev
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

chebyshevTests =
    [ testGroup "ts" ts_tests
    , testGroup "us" us_tests
    , testProperty "Pell's equation" prop_pell's_eqn
    , testProperty "t n == ts !! n" $ \(NonNegative a) -> let n = a `mod` 1000 in t n == ts !! n
    , testProperty "u n == us !! n" $ \(NonNegative a) -> let n = a `mod` 1000 in u n == us !! n
    ]

ts_tests =
    [ testProperty "recurrence" prop_ts_recurrence
    ]

prop_ts_recurrence (NonNegative 0) = ts !! 0 == one
prop_ts_recurrence (NonNegative 1) = ts !! 1 == x
prop_ts_recurrence (NonNegative a) =
    let n = 1 + a `mod` 1200
     in (multPoly (scalePoly 2 x) (ts !! n))
     == addPoly (ts !! (n-1)) (ts !! (n+1))

us_tests =
    [ testProperty "recurrence" prop_us_recurrence
    ]

prop_us_recurrence (NonNegative 0) = us !! 0 == one
prop_us_recurrence (NonNegative 1) = us !! 1 == scalePoly 2 x
prop_us_recurrence (NonNegative a) =
    let n = 1 + a `mod` 1200
     in (multPoly (scalePoly 2 x) (us !! n))
     == addPoly (us !! (n-1)) (us !! (n+1))

prop_pell's_eqn (Positive a) =
    let n = 1 +  a `mod` 800
     in powPoly (ts !! n) 2
     == addPoly one (multPoly (poly BE [1,0,-1]) (powPoly (us !! (n-1)) 2))
