{-# LANGUAGE ParallelListComp #-}
module Math.Polynomial.Bernstein
    ( bernstein
    , evalBernstein
    , bernsteinFit
    , evalBernsteinSeries
    , deCasteljau
    , splitBernsteinSeries
    ) where

import Math.Polynomial
import Data.List

-- |The Bernstein basis polynomials.  The @n@th inner list is a basis for 
-- the polynomials of order @n@ or lower.  The @n@th basis consists of @n@
-- polynomials of order @n@ which sum to @1@, and have roots of varying 
-- multiplicities at @0@ and @1@.
bernstein :: [[Poly Integer]]
bernstein = 
    [ [ scalePoly nCv p `multPoly` q
      | q <- reverse qs
      | p <- ps
      | nCv  <- bico
      ]
    | ps <- tail $ inits [poly BE (1 : zs) | zs <- inits (repeat 0)]
    | qs <- tail $ inits (iterate (multPoly (poly LE [1,-1])) one)
    | bico <- ptri
    ]
    where
        -- pascal's triangle
        ptri = [1] : [ 1 : zipWith (+) row (tail row) ++ [1] | row <- ptri]

-- |@evalBernstein n v x@ evaluates the @v@'th Bernstein polynomial of order @n@
-- at the point @x@.
evalBernstein :: (Integral a, Num b) => a -> a -> b -> b
evalBernstein n v t
    | n < 0 || v > n    = 0
    | otherwise         = fromInteger nCv * t^v * (1-t)^(n-v)
    where
        n' = toInteger n
        v' = toInteger v
        nCv = product [1..n'] `div` (product [1..v'] * product [1..n'-v'])

-- |@bernsteinFit n f@: Approximate a function @f@ as a linear combination of
-- Bernstein polynomials of order @n@.  This approximation converges slowly
-- but uniformly to @f@ on the interval [0,1].
bernsteinFit :: (Fractional b, Integral a) => a -> (b -> b) -> [b]
bernsteinFit n f = [f (fromIntegral v / fromIntegral n) | v <- [0..n]]

-- |Evaluate a polynomial given as a list of @n@ coefficients for the @n@th
-- Bernstein basis.  Roughly:
-- 
-- > evalBernsteinSeries cs = sum (zipWith scalePoly cs (bernstein !! (length cs - 1)))
evalBernsteinSeries :: Num a => [a] -> a -> a
evalBernsteinSeries [] = const 0
evalBernsteinSeries cs = head . last . deCasteljau cs

-- |de Casteljau's algorithm, returning the whole tableau.  Used both for
-- evaluating and splitting polynomials in Bernstein form.
deCasteljau :: Num a => [a] -> a -> [[a]]
deCasteljau [] _ = []
deCasteljau cs t = cs : deCasteljau (zipWith (interp t) cs (tail cs)) t
    where interp t x0 x1 = (1-t)*x0 + t*x1

-- |Given a polynomial in Bernstein form (that is, a list of coefficients
-- for a basis set from 'bernstein', such as is returned by 'bernsteinFit')
-- and a parameter value @x@, split the polynomial into two halves, mapping
-- @[0,x]@ and @[x,1]@ respectively onto @[0,1]@.
--
-- A typical use for this operation would be to split a Bezier curve 
-- (inserting a new knot at @x@).
splitBernsteinSeries :: Num a => [a] -> a -> ([a], [a])
splitBernsteinSeries cs t = (map head betas, map last (reverse betas))
    where
        betas = deCasteljau cs t
