{-# LANGUAGE ExtendedDefaultRules #-}
module Tests.Hermite (hermiteTests) where

import Data.VectorSpace
import Math.Polynomial
import Math.Polynomial.Hermite
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck
import TestUtils

default (Integer, Rational)

hermiteLimit = 500

hermiteTests =
    [ testGroup "probHermite"           probHermite_tests
    , testGroup "physHermite"           physHermite_tests
    , testGroup "evalProbHermite"       evalProbHermite_tests
    , testGroup "evalPhysHermite"       evalPhysHermite_tests
    , testGroup "evalProbHermiteDeriv"  evalProbHermiteDeriv_tests
    , testGroup "evalPhysHermiteDeriv"  evalPhysHermiteDeriv_tests
    ]

probHermite_tests =
    [ testCase     "known values"   case_probHermite_knownValues
    , testProperty "recurrence"     prop_probHermite_recurrence 
    ]

probHermite_knownValues :: [(Int, Poly Integer)]
probHermite_knownValues =
    [ ( 0, one)
    , ( 1, x)
    , ( 2, poly BE [1, 0,  -1])
    , ( 3, poly BE [1, 0,  -3, 0])
    , ( 4, poly BE [1, 0,  -6, 0,  3])
    , ( 5, poly BE [1, 0, -10, 0,  15, 0])
    , ( 6, poly BE [1, 0, -15, 0,  45, 0,   -15])
    , ( 7, poly BE [1, 0, -21, 0, 105, 0,  -105, 0])
    , ( 8, poly BE [1, 0, -28, 0, 210, 0,  -420, 0,  105])
    , ( 9, poly BE [1, 0, -36, 0, 378, 0, -1260, 0,  945, 0])
    , (10, poly BE [1, 0, -45, 0, 630, 0, -3150, 0, 4725, 0, -945])
    ]
case_probHermite_knownValues = sequence_
    [ assertEqual ("probHermite !! " ++ show n) (probHermite !! n) value
    | (n, value) <- probHermite_knownValues
    ]

prop_probHermite_recurrence (Positive n)
    =  probHermite !! (n'+1)
    == multPoly x (probHermite !! n') ^-^ polyDeriv (probHermite !! n')
    where n' = n `mod` hermiteLimit

physHermite_tests =
    [ testCase     "known values"   case_physHermite_knownValues
    , testProperty "recurrence"     prop_physHermite_recurrence 
    ]

physHermite_knownValues :: [(Int, Poly Integer)]
physHermite_knownValues =
    [ ( 0, one)
    , ( 1, poly BE [   2, 0])
    , ( 2, poly BE [   4, 0,     -2])
    , ( 3, poly BE [   8, 0,    -12, 0])
    , ( 4, poly BE [  16, 0,    -48, 0,     12])
    , ( 5, poly BE [  32, 0,   -160, 0,    120, 0])
    , ( 6, poly BE [  64, 0,   -480, 0,    720, 0,    -120])
    , ( 7, poly BE [ 128, 0,  -1344, 0,   3360, 0,   -1680, 0])
    , ( 8, poly BE [ 256, 0,  -3584, 0,  13440, 0,  -13440, 0,   1680])
    , ( 9, poly BE [ 512, 0,  -9216, 0,  48384, 0,  -80640, 0,  30240, 0])
    , (10, poly BE [1024, 0, -23040, 0, 161280, 0, -403200, 0, 302400, 0, -30240])
    ]
case_physHermite_knownValues = sequence_
    [ assertEqual ("physHermite !! " ++ show n) (physHermite !! n) value
    | (n, value) <- physHermite_knownValues
    ]

prop_physHermite_recurrence (Positive n)
    =  physHermite !! (n'+1)
    == scalePoly 2 (multPoly x (physHermite !! n')) ^-^ polyDeriv (physHermite !! n')
    where n' = n `mod` hermiteLimit

evalProbHermite_tests =
    [ testProperty "sane"   prop_evalProbHermite_sane
    ]

prop_evalProbHermite_sane n x
    =  evalProbHermite n' x
    == evalPoly (fmap fromIntegral (probHermite !! n')) x
    where n' = n `mod` hermiteLimit

evalPhysHermite_tests =
    [ testProperty "sane"   prop_evalPhysHermite_sane
    ]

prop_evalPhysHermite_sane n x
    =  evalPhysHermite n' x
    == evalPoly (fmap fromIntegral (physHermite !! n')) x
    where n' = n `mod` hermiteLimit

evalProbHermiteDeriv_tests =
    [ testProperty "sane"   prop_evalProbHermiteDeriv_sane
    ]

prop_evalProbHermiteDeriv_sane n x
    =  evalProbHermiteDeriv n' x
    == evalPolyDeriv (fmap fromIntegral (probHermite !! n')) x
    where n' = n `mod` hermiteLimit

evalPhysHermiteDeriv_tests =
    [ testProperty "sane"   prop_evalPhysHermiteDeriv_sane
    ]

prop_evalPhysHermiteDeriv_sane n x
    =  evalPhysHermiteDeriv n' x
    == evalPolyDeriv (fmap fromIntegral (physHermite !! n')) x
    where n' = n `mod` hermiteLimit

