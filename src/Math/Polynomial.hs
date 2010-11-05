{-# LANGUAGE ParallelListComp, ViewPatterns, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Math.Polynomial
    ( Endianness(..)
    , Poly, poly, polyCoeffs, polyIsZero, polyIsOne
    , zero, one, constPoly, x
    , scalePoly, negatePoly
    , composePoly
    , addPoly, sumPolys, multPoly, powPoly
    , quotRemPoly, quotPoly, remPoly
    , evalPoly, evalPolyDeriv, evalPolyDerivs
    , contractPoly
    , gcdPoly, separateRoots
    , polyDeriv, polyIntegral
    ) where

import Math.Polynomial.Type
import Math.Polynomial.Pretty ({- instance -})

import Data.List
import Data.List.ZipSum

-- |The polynomial \"0\"
zero :: Num a => Poly a
zero = poly LE []

-- |The polynomial \"1\"
one :: Num a => Poly a
one = constPoly 1

-- |The polynomial (in x) \"x\"
x :: Num a => Poly a
x = poly LE [0,1]

-- |Given some constant 'k', construct the polynomial whose value is 
-- constantly 'k'.
constPoly :: Num a => a -> Poly a
constPoly x = poly LE [x]

-- |Given some scalar 's' and a polynomial 'f', computes the polynomial 'g'
-- such that:
-- 
-- > evalPoly g x = s * evalPoly f x
scalePoly :: Num a => a -> Poly a -> Poly a
scalePoly 0 _ = zero
scalePoly s p = fmap (s*) p

-- |Given some polynomial 'f', computes the polynomial 'g' such that:
-- 
-- > evalPoly g x = negate (evalPoly f x)
negatePoly :: Num a => Poly a -> Poly a
negatePoly = fmap negate

-- |Given polynomials 'f' and 'g', computes the polynomial 'h' such that:
-- 
-- > evalPoly h x = evalPoly f x + evalPoly g x
addPoly :: Num a => Poly a -> Poly a -> Poly a
addPoly (polyCoeffs LE ->  a) (polyCoeffs LE ->  b) = poly LE (zipSum a b)

{-# RULES
  "sum Poly"    forall ps. foldl addPoly zero ps = sumPolys ps
  #-}
sumPolys :: Num a => [Poly a] -> Poly a
sumPolys [] = zero
sumPolys ps = poly LE (foldl1 zipSum (map (polyCoeffs LE) ps))

-- |Given polynomials 'f' and 'g', computes the polynomial 'h' such that:
-- 
-- > evalPoly h x = evalPoly f x * evalPoly g x
multPoly :: Num a => Poly a -> Poly a -> Poly a
multPoly (polyCoeffs LE -> xs) (polyCoeffs LE -> ys) = poly LE (multPolyLE xs ys)

-- |(Internal): multiply polynomials in LE order.  O(length xs * length ys).
multPolyLE :: Num a => [a] -> [a] -> [a]
multPolyLE _  []     = []
multPolyLE xs (y:ys) = foldr mul [] xs
    where
        mul 0 bs = 0 : bs
        mul x bs = (x*y) : zipSum (map (x*) ys) bs

-- |Given a polynomial 'f' and exponent 'n', computes the polynomial 'g'
-- such that:
-- 
-- > evalPoly g x = evalPoly f x ^ n
powPoly :: (Num a, Integral b) => Poly a -> b -> Poly a
powPoly _ 0 = poly LE [1]
powPoly p 1 = p
powPoly p n
    | n < 0     = error "powPoly: negative exponent"
    | odd n     = p `multPoly` powPoly p (n-1)
    | otherwise = (\x -> multPoly x x) (powPoly p (n`div`2))

-- |Given polynomials @a@ and @b@, with @b@ not 'zero', computes polynomials
-- @q@ and @r@ such that:
-- 
-- > addPoly (multPoly q b) r == a
quotRemPoly :: Fractional a => Poly a -> Poly a -> (Poly a, Poly a)
quotRemPoly _ b | polyIsZero b = error "quotRemPoly: divide by zero"
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
quotPoly u v
    | polyIsZero v  = error "quotPoly: divide by zero"
    | otherwise     = fst (quotRemPoly u v)
remPoly :: Fractional a => Poly a -> Poly a -> Poly a
remPoly _ b | polyIsZero b = error "remPoly: divide by zero"
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

-- |@composePoly f g@ constructs the polynomial 'h' such that:
-- 
-- > evalPoly h = evalPoly f . evalPoly g
composePoly :: Num a => Poly a -> Poly a -> Poly a
composePoly (polyCoeffs LE -> cs) (polyCoeffs LE -> ds) = poly LE (foldr mul [] cs)
    where
        -- Implementation note: this is a hand-inlining of the following
        -- (with the 'Num' instance in "Math.Polynomial.NumInstance"):
        -- > composePoly f g = evalPoly (fmap constPoly f) g
        -- 
        -- This is a very expensive operation, something like
        -- O(length cs ^ 2 * length ds) I believe. There may be some more 
        -- tricks to improve that, but I suspect there isn't much room for 
        -- improvement. The number of terms in the resulting polynomial is 
        -- O(length cs * length ds) already, and each one is the sum of 
        -- quite a few terms.
        mul c acc = addScalarLE c (multPolyLE acc ds)

-- |(internal) add a scalar to a list of polynomial coefficients in LE order
addScalarLE :: Num a => a -> [a] -> [a]
addScalarLE 0 bs = bs
addScalarLE a [] = [a]
addScalarLE a (b:bs) = (a + b) : bs

-- |Evaluate a polynomial at a point or, equivalently, convert a polynomial
-- to the function it represents.  For example, @evalPoly 'x' = 'id'@ and 
-- @evalPoly ('constPoly' k) = 'const' k.@
evalPoly :: Num a => Poly a -> a -> a
evalPoly (polyCoeffs LE -> cs) 0
    | null cs   = 0
    | otherwise = head cs
evalPoly (polyCoeffs LE -> cs) x = foldr mul 0 cs
    where
        mul c acc = c + acc * x

-- |Evaluate a polynomial and its derivative (respectively) at a point.
evalPolyDeriv :: Num a => Poly a -> a -> (a,a)
evalPolyDeriv (polyCoeffs LE -> cs) x = foldr mul (0,0) cs
    where
        mul c (p, dp) = (p * x + c, dp * x + p)

-- |Evaluate a polynomial and all of its nonzero derivatives at a point.  
-- This is roughly equivalent to:
-- 
-- > evalPolyDerivs p x = map (`evalPoly` x) (takeWhile (not . polyIsZero) (iterate polyDeriv p))
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

-- |@gcdPoly a b@ computes the highest order monic polynomial that is a 
-- divisor of both @a@ and @b@.  If both @a@ and @b@ are 'zero', the 
-- result is undefined.
gcdPoly :: Fractional a => Poly a -> Poly a -> Poly a
gcdPoly a b 
    | polyIsZero b  = if polyIsZero a
        then error "gcdPoly: gcdPoly zero zero is undefined"
        else monic a
    | otherwise     = gcdPoly b (a `remPoly` b)

-- |(internal) Normalize a polynomial so that its highest-order coefficient is 1
monic :: Fractional a => Poly a -> Poly a
monic p = case polyCoeffs BE p of
    []      -> poly BE []
    (c:cs)  -> poly BE (1:map (/c) cs)

-- |Compute the derivative of a polynomial.
polyDeriv :: Num a => Poly a -> Poly a
polyDeriv (polyCoeffs LE -> cs) = poly LE
    [ c * n
    | c <- drop 1 cs
    | n <- iterate (1+) 1
    ]

-- |Compute the definite integral (from 0 to x) of a polynomial.
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
    | polyIsZero q  = error "separateRoots: zero polynomial"
    | polyIsOne q   = [p]
    | otherwise     = p `quotPoly` q : separateRoots q
    where
        q = gcdPoly p (polyDeriv p)
