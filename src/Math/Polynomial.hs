{-# LANGUAGE ParallelListComp, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Math.Polynomial
    ( Endianness(..)
    , Poly, poly, polyCoeffs, polyIsZero, polyIsOne
    , zero, one, x
    , scalePoly, negatePoly
    , addPoly, sumPolys, multPoly, powPoly
    , quotRemPoly, quotPoly, remPoly
    , evalPoly, evalPolyDeriv, evalPolyDerivs
    , contractPoly
    , gcdPoly, separateRoots
    , polyDeriv, polyIntegral
    ) where

import Math.Polynomial.Type
import Math.Polynomial.Pretty ({- instance -})

import Data.AdditiveGroup
import Data.List
import Data.List.ZipSum

zero :: Num a => Poly a
zero = zeroV

one :: Num a => Poly a
one = poly LE [1]

x :: Num a => Poly a
x = poly LE [0,1]

scalePoly :: Num a => a -> Poly a -> Poly a
scalePoly s p = fmap (s*) p

negatePoly :: Num a => Poly a -> Poly a
negatePoly = negateV

addPoly :: Num a => Poly a -> Poly a -> Poly a
addPoly = (^+^)

sumPolys :: Num a => [Poly a] -> Poly a
sumPolys [] = zero
sumPolys ps = poly LE (foldl1 zipSum (map (polyCoeffs LE) ps))

multPoly :: Num a => Poly a -> Poly a -> Poly a
multPoly (polyCoeffs LE -> xs) (polyCoeffs LE -> ys) = poly LE $ multX ys
    where
        multX (0:ys) = 0:multX ys
        multX ys = foldl zipSum []
            [ shift ++ map (x *) ys
            | (x, shift) <- zip xs (inits (repeat 0))
            , x /= 0
            ]

powPoly :: (Num a, Integral b) => Poly a -> b -> Poly a
powPoly _ 0 = poly LE [1]
powPoly p 1 = p
powPoly p n
    | odd n     = p `multPoly` powPoly p (n-1)
    | otherwise = (\x -> multPoly x x) (powPoly p (n`div`2))

quotRemPoly :: Fractional a => Poly a -> Poly a -> (Poly a, Poly a)
quotRemPoly (polyCoeffs BE -> u) (polyCoeffs BE -> v)
    = go [] u (length u - length v)
    where
        v0  | null v    = 0
            | otherwise = head v
        go q u n
            | null u || n < 0   = (poly LE q, poly BE u)
            | otherwise         = go (q0:q) u' (n-1)
            where
                q0 = head u / v0
                u' = tail (zipSum u (map (negate q0 *) v))

quotPoly :: Fractional a => Poly a -> Poly a -> Poly a
quotPoly u v = fst (quotRemPoly u v)
remPoly :: Fractional a => Poly a -> Poly a -> Poly a
remPoly (polyCoeffs BE -> u) (polyCoeffs BE -> v)
    = go u (length u - length v)
    where
        v0  | null v    = 0
            | otherwise = head v
        go u n
            | null u || n < 0   = poly BE u
            | otherwise         = go u' (n-1)
            where
                q0 = head u / v0
                u' = tail (zipSum u (map (negate q0 *) v))


evalPoly :: Num a => Poly a -> a -> a
evalPoly (polyCoeffs LE -> cs) x = foldr mul 0 cs
    where
        mul c acc = c + acc * x

evalPolyDeriv :: Num a => Poly a -> a -> (a,a)
evalPolyDeriv (polyCoeffs LE -> cs) x = foldr mul (0,0) cs
    where
        mul c (p, dp) = (p * x + c, dp * x + p)

evalPolyDerivs :: Num a => Poly a -> a -> [a]
evalPolyDerivs (polyCoeffs LE -> cs) x = trunc . zipWith (*) factorials $ foldr mul [] cs
    where
        trunc list = zipWith const list cs
        factorials = scanl (*) 1 (iterate (+1) 1)
        mul c pds@(p:pd) = (p * x + c) : map (x *) pd `zipSum` pds
        mul c [] = [c]

-- |\"Contract\" a polynomial by attempting to divide out a root.
--
-- @contractPoly p a@ returns @(q,r)@ such that @q*(x-a) + r == p@
contractPoly :: Num a => Poly a -> a -> (Poly a, a)
contractPoly (polyCoeffs LE -> cs) a = (poly LE q, r)
    where
        cut remainder swap = (swap + remainder * a, remainder)
        (r,q) = mapAccumR cut 0 cs

gcdPoly :: Fractional a => Poly a -> Poly a -> Poly a
gcdPoly a (polyIsZero -> True)      =  monic a
gcdPoly a b                         =  gcdPoly b (a `remPoly` b)

-- |Normalize a polynomial so that its highest-order coefficient is 1
monic :: Fractional a => Poly a -> Poly a
monic p = case polyCoeffs BE p of
    []      -> poly BE []
    (c:cs)  -> poly BE (1:map (/c) cs)


polyDeriv :: Num a => Poly a -> Poly a
polyDeriv (polyCoeffs LE -> cs) = poly LE
    [ c * n
    | c <- tail cs
    | n <- iterate (1+) 1
    ]

polyIntegral :: Fractional a => Poly a -> Poly a
polyIntegral (polyCoeffs LE -> cs) = poly LE $ 0 :
    [ c / n
    | c <- cs
    | n <- iterate (1+) 1
    ]

-- |Separate a polynomial into a set of factors none of which have
-- multiple roots, and the product of which is the original polynomial.
-- Note that if division is not exact, it may fail to separate roots.  
-- Rational coefficients is a good idea.
--
-- Useful when applicable as a way to simplify root-finding problems.
separateRoots :: Fractional a => Poly a -> [Poly a]
separateRoots p
    | polyIsOne q   = [p]
    | otherwise     = p `quotPoly` q : separateRoots q
    where
        q = gcdPoly p (polyDeriv p)
