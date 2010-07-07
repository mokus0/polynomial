{-# LANGUAGE ParallelListComp #-}
module Math.Polynomial.Lagrange
    ( lagrangeBasis
    , lagrange
    , lagrangeWeights
    ) where

import Math.Polynomial

-- given a list, return one list containing each element of the original list
-- paired with all the other elements of the list.
select :: [a] -> [(a,[a])]
select [] = []
select (x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <- select xs]

-- |Returns the Lagrange basis set of polynomials associated with a set of 
-- points. This is the set of polynomials each of which is @1@ at its 
-- corresponding point in the input list and @0@ at all others.
--
-- These polynomials are especially convenient, mathematically, for 
-- interpolation.  The interpolating polynomial for a set of points  @(x,y)@ 
-- is given by using the @y@s as coefficients for the basis given by 
-- @lagrangeBasis xs@.  Computationally, this is not an especially stable 
-- procedure though.  'Math.Polynomial.Interpolation.lagrangePolyFit'
-- implements a slightly better algorithm based on the same idea.  
-- 
-- Generally it is better to not compute the coefficients at all.  
-- 'Math.Polynomial.Interpolation.polyInterp' evaluates the interpolating
-- polynomial directly, and is both quicker and more stable than any method
-- I know of that computes the coefficients.
lagrangeBasis :: Fractional a => [a] -> [Poly a]
lagrangeBasis xs =
    [ foldl1 multPoly
        [ if q /= 0
            then poly LE [negate x_j/q, 1/q]
            else error ("lagrangeBasis: duplicate root: " ++ show x_i)
        | x_j <- otherXs
        , let q = x_i - x_j
        ]
    | (x_i, otherXs) <- select xs
    ]

-- |Construct the Lagrange "master polynomial" for the Lagrange barycentric form:
-- That is, the monic polynomial with a root at each point in the input list.
lagrange :: Num a => [a] -> Poly a
lagrange [] = one
lagrange xs = foldl1 multPoly
    [ poly LE [negate x_i, 1]
    | x_i <- xs
    ]

-- |Compute the weights associated with each abscissa in the Lagrange
-- barycentric form.
lagrangeWeights :: Fractional a => [a] -> [a]
lagrangeWeights xs = 
    [ recip $ product
        [ x_i - x_j
        | x_j <- otherXs
        ]
    | (x_i, otherXs) <- select xs
    ]
