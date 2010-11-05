{-# LANGUAGE RankNTypes #-}
module Bench.Core (coreTests) where

import TestData

import Criterion
import Control.DeepSeq
import Control.Monad
import Data.Ratio ((%))
import Math.Polynomial
import Data.List (foldl', nub)

-- special version of a few functions that won't barf on 0
quotPoly' x y | polyIsZero y            = zero
              | otherwise               = quotPoly x y
remPoly'  x y | polyIsZero y            = zero
              | otherwise               = remPoly x y
gcdPoly'  x y | all polyIsZero [x,y]    = zero
              | otherwise               = gcdPoly x y
separateRoots' p
    | polyIsZero p  = []
    | otherwise     = separateRoots p

coreTests :: [Benchmark]
coreTests = evalData `seq`
    [ bgroup "addPoly"      (binOpTests    addPoly)
    , bgroup "multPoly"     (binOpTests    multPoly)
    , bgroup "composePoly"  (binOpTests    composePoly)
    , bgroup "quotPoly"     (binOpTestsF   quotPoly')
    , bgroup "remPoly"      (binOpTestsF   remPoly')
    , bgroup "gcdPoly"      (binOpTestsF   gcdPoly')
    , bgroup "powPoly"
            [ bgroup "2"  (unaryOpTests (rnf . flip powPoly 2 ))
            , bgroup "5"  (unaryOpTests (rnf . flip powPoly 5 ))
            , bgroup "23" (unaryOpTests (rnf . flip powPoly 23))
            , bgroup "75" (unaryOpTests (rnf . flip powPoly 75))
            ]
    , bgroup "evalPoly"
            [ bgroup "0"    (unaryOpTests (rnf . flip evalPoly 0     ))
            , bgroup "7"    (unaryOpTests (rnf . flip evalPoly 7     ))
            , bgroup "-999" (unaryOpTests (rnf . flip evalPoly (-999)))
            , bgroup "2^80" (unaryOpTests (rnf . flip evalPoly (2^80)))
            ]
    , bgroup "negatePoly"    (unaryOpTests  (rnf . negatePoly))
    , bgroup "polyDeriv"     (unaryOpTests  (rnf . polyDeriv))
    , bgroup "polyIntegral"  (unaryOpTestsF (rnf . polyIntegral))
    , bgroup "separateRoots" (unaryOpTestsF (rnf . separateRoots'))
    ]

unaryOpTests :: (forall a. (Num a, NFData a) => Poly a -> ()) -> [Benchmark]
unaryOpTests op = 
    [ bgroup "Int"     (testOp op intPolys)
    , bgroup "Float"   (testOp op floatPolys)
    , bgroup "Double"  (testOp op doublePolys)
    , bgroup "Integer" (testOp op integerPolys)
    ]

unaryOpTestsF :: (forall a. (Fractional a, NFData a) => Poly a -> ()) -> [Benchmark]
unaryOpTestsF op = 
    [ bgroup "Float"   (testOp op floatPolys)
    , bgroup "Double"  (testOp op doublePolys)
    ]

testOp op xs = 
    [ bench (showTags x) (nf op (untag x))
    | x <- xs
    ]

binOpTests :: (forall a. (Num a, NFData a) => Poly a -> Poly a -> Poly a) -> [Benchmark]
binOpTests op = 
    [ bgroup "Int"     (testOp (uncurry op) intPolyPairs)
    , bgroup "Float"   (testOp (uncurry op) floatPolyPairs)
    , bgroup "Double"  (testOp (uncurry op) doublePolyPairs)
    , bgroup "Integer" (testOp (uncurry op) integerPolyPairs)
    ]

binOpTestsF :: (forall a. (Fractional a, NFData a) => Poly a -> Poly a -> Poly a) -> [Benchmark]
binOpTestsF op = 
    [ bgroup "Float"   (testOp (uncurry op) floatPolyPairs)
    , bgroup "Double"  (testOp (uncurry op) doublePolyPairs)
    ]

------ Pre-computed random data (significantly more than is actually needed) ------
evalData :: ()
evalData = foldl' seq () 
    [ rnf floatPolys,   rnf floatPolyPairs
    , rnf doublePolys,  rnf doublePolyPairs
    , rnf intPolys,     rnf intPolyPairs
    , rnf integerPolys, rnf integerPolyPairs
    ]

floatPolys :: [Tagged (Poly Float)]
floatPolys = allPolys

floatPolyPairs :: [Tagged (Poly Float, Poly Float)]
floatPolyPairs = testPairs

doublePolys :: [Tagged (Poly Double)]
doublePolys = allPolys

doublePolyPairs :: [Tagged (Poly Double, Poly Double)]
doublePolyPairs = testPairs

intPolys :: [Tagged (Poly Int)]
intPolys = allPolys

intPolyPairs :: [Tagged (Poly Int, Poly Int)]
intPolyPairs = testPairs

integerPolys :: [Tagged (Poly Integer)]
integerPolys = allPolys

integerPolyPairs :: [Tagged (Poly Integer, Poly Integer)]
integerPolyPairs = testPairs

testPairs :: Num a => [Tagged (Poly a, Poly a)]
testPairs = arbitrarySubset' 10 (liftM2 argPair allPolys allPolys)
