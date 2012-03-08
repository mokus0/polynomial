{-# LANGUAGE ParallelListComp, ViewPatterns, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- TODO: update all haddock comments
-- |Same general interface as Math.Polynomial, but using AdditiveGroup, 
-- VectorSpace, etc., instead of Num where sensible.
module Math.Polynomial.VectorSpace
    ( Endianness(..)
    , Poly, poly, polyDegree
    , vPolyCoeffs, polyIsZero, polyIsOne
    , zero, one, constPoly, x
    , scalePoly, negatePoly
    , composePolyWith
    , addPoly, sumPolys, multPolyWith, powPolyWith
    , quotRemPolyWith, quotPolyWith, remPolyWith
    , evalPoly, evalPolyDeriv, evalPolyDerivs
    , contractPoly
    , monicPolyWith
    , gcdPolyWith
    , polyDeriv, polyDerivs, polyIntegral
    ) where

import Math.Polynomial.Type hiding (poly, polyDegree, polyIsZero)
import Math.Polynomial.Pretty ({- instance -})

import Data.List
import Data.List.ZipSum

import Data.VectorSpace

vPolyN :: (Eq a, AdditiveGroup a) => Int -> Endianness -> [a] -> Poly a
vPolyN n e = vTrim . rawListPolyN n e

poly :: (Eq a, AdditiveGroup a) => Endianness -> [a] -> Poly a
poly e = vTrim . rawListPoly e

polyDegree :: (Eq a, AdditiveGroup a) => Poly a -> Int
polyDegree p = rawPolyDegree (vTrim p)

polyIsZero :: (Eq a, AdditiveGroup a) => Poly a -> Bool
polyIsZero = null . rawPolyCoeffs . vTrim

-- |The polynomial \"1\"
one :: (Num a, Eq a) => Poly a
one = polyN 1 LE [1]

-- |The polynomial (in x) \"x\"
x :: (Num a, Eq a) => Poly a
x = polyN 2 LE [0,1]

-- |Given some constant 'k', construct the polynomial whose value is 
-- constantly 'k'.
constPoly :: (Eq a, AdditiveGroup a) => a -> Poly a
constPoly x = vPolyN 1 LE [x]

-- |Given some scalar 's' and a polynomial 'f', computes the polynomial 'g'
-- such that:
-- 
-- > evalPoly g x = s * evalPoly f x
scalePoly :: (Eq a, VectorSpace a, AdditiveGroup (Scalar a), Eq (Scalar a)) 
    => Scalar a -> Poly a -> Poly a
scalePoly = (*^)

-- |Given some polynomial 'f', computes the polynomial 'g' such that:
-- 
-- > evalPoly g x = negate (evalPoly f x)
negatePoly :: (AdditiveGroup a, Eq a) => Poly a -> Poly a
negatePoly = vTrim . rawMapPoly negateV

-- |Given polynomials 'f' and 'g', computes the polynomial 'h' such that:
-- 
-- > evalPoly h x = evalPoly f x + evalPoly g x
addPoly :: (AdditiveGroup a, Eq a) => Poly a -> Poly a -> Poly a
addPoly p@(vPolyCoeffs LE ->  a) q@(vPolyCoeffs LE ->  b) = vPolyN n LE (zipSumV a b)
    where n = max (rawPolyLength p) (rawPolyLength q)

{-# RULES
  "sum Poly"    forall ps. foldl addPoly zero ps = sumPolys ps
  #-}
sumPolys :: (AdditiveGroup a, Eq a) => [Poly a] -> Poly a
sumPolys [] = zero
sumPolys ps = poly LE (foldl1 zipSumV (map (vPolyCoeffs LE) ps))

-- |Given polynomials 'f' and 'g', computes the polynomial 'h' such that:
-- 
-- > evalPoly h x = evalPoly f x * evalPoly g x
multPolyWith :: (AdditiveGroup a, Eq a) => (a -> a -> a) -> Poly a -> Poly a -> Poly a
multPolyWith multiplyV p@(vPolyCoeffs LE -> xs) q@(vPolyCoeffs LE -> ys) = vPolyN n LE (multPolyWithLE multiplyV xs ys)
    where n = 1 + rawPolyDegree p + rawPolyDegree q

-- |(Internal): multiply polynomials in LE order.  O(length xs * length ys).
multPolyWithLE :: (AdditiveGroup a, Eq a) => (a -> a -> a) -> [a] -> [a] -> [a]
multPolyWithLE _         _  []     = []
multPolyWithLE multiplyV xs (y:ys) = foldr mul [] xs
    where
        mul x bs
            | x == zeroV    = zeroV : bs
            | otherwise     = (multiplyV x y) : zipSumV (map (multiplyV x) ys) bs

-- |Given a polynomial 'f' and exponent 'n', computes the polynomial 'g'
-- such that:
-- 
-- > evalPoly g x = evalPoly f x ^ n
powPolyWith :: (AdditiveGroup a, Eq a, Integral b) => a -> (a -> a -> a) -> Poly a -> b -> Poly a
powPolyWith one multiplyV p n
    | n < 0     = error "powPolyWith: negative exponent"
    | otherwise = powPoly p n
    where
        multPoly = multPolyWith multiplyV
        powPoly p 0 = constPoly one
        powPoly p 1 = p
        powPoly p n 
            | odd n     = p `multPoly` powPoly p (n-1)
            | otherwise = (\x -> multPoly x x) (powPoly p (n`div`2))

-- |Given polynomials @a@ and @b@, with @b@ not 'zero', computes polynomials
-- @q@ and @r@ such that:
-- 
-- > addPoly (multPoly q b) r == a
quotRemPolyWith :: (AdditiveGroup a, Eq a) => (a -> a -> a) -> (a -> a -> a) -> Poly a -> Poly a -> (Poly a, Poly a)
quotRemPolyWith _ _ _ b | polyIsZero b = error "quotRemPoly: divide by zero"
quotRemPolyWith multiplyV divideV p@(vPolyCoeffs BE -> u) q@(vPolyCoeffs BE -> v)
    = go [] u (polyDegree p - polyDegree q)
    where
        v0  | null v    = zeroV
            | otherwise = head v
        go q u n
            | null u || n < 0   = (poly LE q, poly BE u)
            | otherwise         = go (q0:q) u' (n-1)
            where
                q0 = divideV (head u) v0
                u' = tail (zipSumV u (map (multiplyV (negateV q0)) v))

quotPolyWith :: (AdditiveGroup a, Eq a) => (a -> a -> a) -> (a -> a -> a) -> Poly a -> Poly a -> Poly a
quotPolyWith multiplyV divideV u v
    | polyIsZero v  = error "quotPoly: divide by zero"
    | otherwise     = fst (quotRemPolyWith multiplyV divideV u v)
remPolyWith :: (AdditiveGroup a, Eq a) => (a -> a -> a) -> (a -> a -> a) -> Poly a -> Poly a -> Poly a
remPolyWith _ _ _ b | polyIsZero b = error "remPoly: divide by zero"
remPolyWith multiplyV divideV (vPolyCoeffs BE -> u) (vPolyCoeffs BE -> v)
    = go u (length u - length v)
    where
        v0  | null v    = zeroV
            | otherwise = head v
        go u n
            | null u || n < 0   = poly BE u
            | otherwise         = go u' (n-1)
            where
                q0 = divideV (head u) v0
                u' = tail (zipSumV u (map (multiplyV (negateV q0)) v))

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
composePolyWith :: (AdditiveGroup a, Eq a) => (a -> a -> a) -> Poly a -> Poly a -> Poly a
composePolyWith multiplyV (vPolyCoeffs LE -> cs) (vPolyCoeffs LE -> ds) = poly LE (foldr mul [] cs)
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
        mul c acc = addScalarLE c (multPolyWithLE multiplyV acc ds)

-- |(internal) add a scalar to a list of polynomial coefficients in LE order
addScalarLE :: (AdditiveGroup a, Eq a) => a -> [a] -> [a]
addScalarLE a bs | a == zeroV = bs
addScalarLE a [] = [a]
addScalarLE a (b:bs) = (a ^+^ b) : bs

-- |Evaluate a polynomial at a point or, equivalently, convert a polynomial
-- to the function it represents.  For example, @evalPoly 'x' = 'id'@ and 
-- @evalPoly ('constPoly' k) = 'const' k.@
evalPoly :: (VectorSpace a, Eq a, AdditiveGroup (Scalar a), Eq (Scalar a)) => Poly a -> Scalar a -> a
evalPoly (vPolyCoeffs LE -> cs) x
    | x == zeroV =
        if null cs
            then zeroV
            else head cs
    | otherwise = foldr mul zeroV cs
    where
        mul c acc = c ^+^ acc ^* x

-- |Evaluate a polynomial and its derivative (respectively) at a point.
evalPolyDeriv :: (VectorSpace a, Eq a) => Poly a -> Scalar a -> (a,a)
evalPolyDeriv (vPolyCoeffs LE -> cs) x = foldr mul (zeroV, zeroV) cs
    where
        mul c (p, dp) = ((x *^ p) ^+^ c, (x *^ dp) ^+^ p)

-- |Evaluate a polynomial and all of its nonzero derivatives at a point.  
-- This is roughly equivalent to:
-- 
-- > evalPolyDerivs p x = map (`evalPoly` x) (takeWhile (not . polyIsZero) (iterate polyDeriv p))
evalPolyDerivs :: (VectorSpace a, Eq a, Num (Scalar a)) => Poly a -> Scalar a -> [a]
evalPolyDerivs (vPolyCoeffs LE -> cs) x = trunc . zipWith (*^) factorials $ foldr mul [] cs
    where
        trunc list = zipWith const list cs
        factorials = scanl (*) 1 (iterate (+1) 1)
        mul c pds@(p:pd) = (x *^ p ^+^ c) : map (x *^) pd `zipSumV` pds
        mul c [] = [c]

-- |\"Contract\" a polynomial by attempting to divide out a root.
--
-- @contractPoly p a@ returns @(q,r)@ such that @q*(x-a) + r == p@
contractPoly :: (VectorSpace a, Eq a) => Poly a -> Scalar a -> (Poly a, a)
contractPoly p@(vPolyCoeffs LE -> cs) a = (vPolyN n LE q, r)
    where
        n = rawPolyLength p
        cut remainder swap = (swap ^+^ (a *^ remainder), remainder)
        (r,q) = mapAccumR cut zeroV cs

-- |@gcdPoly a b@ computes the highest order monic polynomial that is a 
-- divisor of both @a@ and @b@.  If both @a@ and @b@ are 'zero', the 
-- result is undefined.
gcdPolyWith :: (AdditiveGroup a, Eq a) => a -> (a -> a -> a) -> (a -> a -> a) -> Poly a -> Poly a -> Poly a
gcdPolyWith oneV multiplyV divideV a b 
    | polyIsZero b  = if polyIsZero a
        then error "gcdPolyWith: gcdPoly zero zero is undefined"
        else monicPolyWith oneV divideV a
    | otherwise     = gcdPolyWith oneV multiplyV divideV b (a `remPoly` b)
    where remPoly = remPolyWith multiplyV divideV

-- |Normalize a polynomial so that its highest-order coefficient is 1
monicPolyWith :: (AdditiveGroup a, Eq a) => a -> (a -> a -> a) -> Poly a -> Poly a
monicPolyWith oneV divideV p = case vPolyCoeffs BE p of
    []      -> vPolyN n BE []
    (c:cs)  -> vPolyN n BE (oneV : map (`divideV` c) cs)
    where n = rawPolyLength p

-- |Compute the derivative of a polynomial.
polyDeriv :: (VectorSpace a, Eq a, Num (Scalar a)) => Poly a -> Poly a
polyDeriv p@(vPolyCoeffs LE -> cs) = vPolyN (rawPolyDegree p) LE
    [ n *^ c
    | c <- drop 1 cs
    | n <- iterate (1+) 1
    ]

-- |Compute all nonzero derivatives of a polynomial, starting with its 
-- \"zero'th derivative\", the original polynomial itself.
polyDerivs :: (VectorSpace a, Eq a, Num (Scalar a)) => Poly a -> [Poly a]
polyDerivs p = take (1 + polyDegree p) (iterate polyDeriv p)


-- |Compute the definite integral (from 0 to x) of a polynomial.
polyIntegral :: (VectorSpace a, Eq a, Fractional (Scalar a)) => Poly a -> Poly a
polyIntegral p@(vPolyCoeffs LE -> cs) = vPolyN (1 + rawPolyLength p) LE $ zeroV :
    [ c ^/ n
    | c <- cs
    | n <- iterate (1+) 1
    ]
