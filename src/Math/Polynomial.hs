{-# LANGUAGE ParallelListComp, ViewPatterns, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Math.Polynomial
    ( Endianness(..)
    , Poly, poly, polyDegree
    , polyCoeffs, polyIsZero, polyIsOne
    , zero, one, constPoly, x
    , scalePoly, negatePoly
    , composePoly
    , addPoly, sumPolys, multPoly, powPoly
    , quotRemPoly, quotPoly, remPoly
    , evalPoly, evalPolyDeriv, evalPolyDerivs
    , contractPoly
    , monicPoly
    , gcdPoly, separateRoots
    , polyDeriv, polyDerivs, polyIntegral
    ) where

import Math.Polynomial.Type
import Math.Polynomial.Pretty ({- instance -})

import Math.Polynomial.VectorSpace (one, x) -- to re-export
import qualified Math.Polynomial.VectorSpace as VS
import Data.VectorSpace.WrappedNum

-- |Given some constant 'k', construct the polynomial whose value is 
-- constantly 'k'.
constPoly :: (Num a, Eq a) => a -> Poly a
constPoly x = unwrapPoly (VS.constPoly (WrapNum x))

-- |Given some scalar 's' and a polynomial 'f', computes the polynomial 'g'
-- such that:
-- 
-- > evalPoly g x = s * evalPoly f x
scalePoly :: (Num a, Eq a) => a -> Poly a -> Poly a
scalePoly x f = unwrapPoly (VS.scalePoly (WrapNum x) (wrapPoly f))

-- |Given some polynomial 'f', computes the polynomial 'g' such that:
-- 
-- > evalPoly g x = negate (evalPoly f x)
negatePoly :: (Num a, Eq a) => Poly a -> Poly a
negatePoly f = unwrapPoly (VS.negatePoly (wrapPoly f))

-- |Given polynomials 'f' and 'g', computes the polynomial 'h' such that:
-- 
-- > evalPoly h x = evalPoly f x + evalPoly g x
addPoly :: (Num a, Eq a) => Poly a -> Poly a -> Poly a
addPoly p q = unwrapPoly (VS.addPoly (wrapPoly p) (wrapPoly q))

{-# RULES
  "sum Poly"    forall ps. foldl addPoly zero ps = sumPolys ps
  #-}
sumPolys :: (Num a, Eq a) => [Poly a] -> Poly a
sumPolys ps = unwrapPoly (VS.sumPolys (map wrapPoly ps))

-- |Given polynomials 'f' and 'g', computes the polynomial 'h' such that:
-- 
-- > evalPoly h x = evalPoly f x * evalPoly g x
multPoly :: (Num a, Eq a) => Poly a -> Poly a -> Poly a
multPoly p q = unwrapPoly (VS.multPolyWith (*) (wrapPoly p) (wrapPoly q))

-- |Given a polynomial 'f' and exponent 'n', computes the polynomial 'g'
-- such that:
-- 
-- > evalPoly g x = evalPoly f x ^ n
powPoly :: (Num a, Eq a, Integral b) => Poly a -> b -> Poly a
powPoly p n = unwrapPoly (VS.powPolyWith 1 (*) (wrapPoly p) n)

-- |Given polynomials @a@ and @b@, with @b@ not 'zero', computes polynomials
-- @q@ and @r@ such that:
-- 
-- > addPoly (multPoly q b) r == a
quotRemPoly :: (Fractional a, Eq a) => Poly a -> Poly a -> (Poly a, Poly a)
quotRemPoly u v = (unwrapPoly q, unwrapPoly r)
    where
        ~(q, r) = VS.quotRemPolyWith (*) (/) (wrapPoly u) (wrapPoly v)

quotPoly :: (Fractional a, Eq a) => Poly a -> Poly a -> Poly a
quotPoly u v = unwrapPoly (VS.quotPolyWith (*) (/) (wrapPoly u) (wrapPoly v))

remPoly :: (Fractional a,  Eq a) => Poly a -> Poly a -> Poly a
remPoly u v = unwrapPoly (VS.remPolyWith (*) (/) (wrapPoly u) (wrapPoly v))

-- |@composePoly f g@ constructs the polynomial 'h' such that:
-- 
-- > evalPoly h = evalPoly f . evalPoly g
-- 
-- This is a very expensive operation and, in general, returns a polynomial 
-- that is quite a bit more expensive to evaluate than @f@ and @g@ together
-- (because it is of a much higher order than either).  Unless your 
-- polynomials are quite small or you are quite certain you need the
-- coefficients of the composed polynomial, it is recommended that you 
-- simply evaluate @f@ and @g@ and explicitly compose the resulting 
-- functions.  This will usually be much more efficient.
composePoly :: (Num a, Eq a) => Poly a -> Poly a -> Poly a
composePoly p q = unwrapPoly (VS.composePolyWith (*) (wrapPoly p) (wrapPoly q))

-- |Evaluate a polynomial at a point or, equivalently, convert a polynomial
-- to the function it represents.  For example, @evalPoly 'x' = 'id'@ and 
-- @evalPoly ('constPoly' k) = 'const' k.@
evalPoly :: (Num a, Eq a) => Poly a -> a -> a
evalPoly f x = unwrapNum (VS.evalPoly (wrapPoly f) (WrapNum x))

-- |Evaluate a polynomial and its derivative (respectively) at a point.
evalPolyDeriv :: (Num a, Eq a) => Poly a -> a -> (a,a)
evalPolyDeriv f x = (unwrapNum y, unwrapNum y')
    where
        ~(y, y') = VS.evalPolyDeriv (wrapPoly f) (WrapNum x)

-- |Evaluate a polynomial and all of its nonzero derivatives at a point.  
-- This is roughly equivalent to:
-- 
-- > evalPolyDerivs p x = map (`evalPoly` x) (takeWhile (not . polyIsZero) (iterate polyDeriv p))
evalPolyDerivs :: (Num a, Eq a) => Poly a -> a -> [a]
evalPolyDerivs f x = map unwrapNum (VS.evalPolyDerivs (wrapPoly f) (WrapNum x))

-- |\"Contract\" a polynomial by attempting to divide out a root.
--
-- @contractPoly p a@ returns @(q,r)@ such that @q*(x-a) + r == p@
contractPoly :: (Num a, Eq a) => Poly a -> a -> (Poly a, a)
contractPoly p a = (unwrapPoly q, unwrapNum r)
    where
        (q, r) = VS.contractPoly (wrapPoly p) (WrapNum a)

-- |@gcdPoly a b@ computes the highest order monic polynomial that is a 
-- divisor of both @a@ and @b@.  If both @a@ and @b@ are 'zero', the 
-- result is undefined.
gcdPoly :: (Fractional a, Eq a) => Poly a -> Poly a -> Poly a
gcdPoly a b = unwrapPoly (VS.gcdPolyWith 1 (*) (/) (wrapPoly a) (wrapPoly b))

-- |Normalize a polynomial so that its highest-order coefficient is 1
monicPoly :: (Fractional a, Eq a) => Poly a -> Poly a
monicPoly p = unwrapPoly (VS.monicPolyWith 1 (/) (wrapPoly p))

-- |Compute the derivative of a polynomial.
polyDeriv :: (Num a, Eq a) => Poly a -> Poly a
polyDeriv p = unwrapPoly (VS.polyDeriv (wrapPoly p))

-- |Compute all nonzero derivatives of a polynomial, starting with its 
-- \"zero'th derivative\", the original polynomial itself.
polyDerivs :: (Num a, Eq a) => Poly a -> [Poly a]
polyDerivs p = map unwrapPoly (VS.polyDerivs (wrapPoly p))

-- |Compute the definite integral (from 0 to x) of a polynomial.
polyIntegral :: (Fractional a, Eq a) => Poly a -> Poly a
polyIntegral p = unwrapPoly (VS.polyIntegral (wrapPoly p))

-- |Separate a nonzero polynomial into a set of factors none of which have
-- multiple roots, and the product of which is the original polynomial.
-- Note that if division is not exact, it may fail to separate roots.  
-- Rational coefficients is a good idea.
--
-- Useful when applicable as a way to simplify root-finding problems.
separateRoots :: (Fractional a, Eq a) => Poly a -> [Poly a]
separateRoots p
    | polyIsZero q  = error "separateRoots: zero polynomial"
    | polyIsOne q   = [p]
    | otherwise     = p `quotPoly` q : separateRoots q
    where
        q = gcdPoly p (polyDeriv p)
