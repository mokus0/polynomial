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

coreTests :: [Benchmark]
coreTests = evalData `seq`
    [ bgroup "addPoly"      (binOpTests    addPoly)
    , bgroup "multPoly"     (binOpTests    multPoly)
    , bgroup "composePoly"  (binOpTests    composePoly)
    , bgroup "quotPoly"     (binOpTestsF   quotPoly')
    , bgroup "remPoly"      (binOpTestsF   remPoly')
    , bgroup "gcdPoly"      (binOpTestsF   gcdPoly')
    , bgroup "negatePoly"   (unaryOpTests  negatePoly)
    , bgroup "polyDeriv"    (unaryOpTests  polyDeriv)
    , bgroup "polyIntegral" (unaryOpTestsF polyIntegral)
    ]

unaryOpTests :: (forall a. (Num a, NFData a) => Poly a -> Poly a) -> [Benchmark]
unaryOpTests op = concat
    [ testOp op intPolys
    , testOp op floatPolys
    , testOp op doublePolys
    , testOp op integerPolys
    ]

unaryOpTestsF :: (forall a. (Fractional a, NFData a) => Poly a -> Poly a) -> [Benchmark]
unaryOpTestsF op = concat
    [ testOp op floatPolys
    , testOp op doublePolys
    ]

testOp op xs = 
    [ bench (showTags x) (nf op (untag x))
    | x <- xs
    ]

binOpTests :: (forall a. (Num a, NFData a) => Poly a -> Poly a -> Poly a) -> [Benchmark]
binOpTests op = concat
    [ testOp (uncurry op) intPolyPairs
    , testOp (uncurry op) floatPolyPairs
    , testOp (uncurry op) doublePolyPairs
    , testOp (uncurry op) integerPolyPairs
    ]

binOpTestsF :: (forall a. (Fractional a, NFData a) => Poly a -> Poly a -> Poly a) -> [Benchmark]
binOpTestsF op = concat
    [ testOp (uncurry op) floatPolyPairs
    , testOp (uncurry op) doublePolyPairs
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
floatPolys = map (tag_ "Float" >>) allPolys

floatPolyPairs :: [Tagged (Poly Float, Poly Float)]
floatPolyPairs = map (tag_ "Float" >>) testPairs

doublePolys :: [Tagged (Poly Double)]
doublePolys = map (tag_ "Double" >>) allPolys

doublePolyPairs :: [Tagged (Poly Double, Poly Double)]
doublePolyPairs = map (tag_ "Double" >>) testPairs

intPolys :: [Tagged (Poly Int)]
intPolys = map (tag_ "Int" >>) allPolys

intPolyPairs :: [Tagged (Poly Int, Poly Int)]
intPolyPairs = map (tag_ "Int" >>) testPairs

integerPolys :: [Tagged (Poly Integer)]
integerPolys = map (tag_ "Integer" >>) allPolys

integerPolyPairs :: [Tagged (Poly Integer, Poly Integer)]
integerPolyPairs = map (tag_ "Integer" >>) testPairs

testPairs :: Num a => [Tagged (Poly a, Poly a)]
testPairs = arbitrarySubset' 10 (liftM2 argPair allPolys allPolys)
