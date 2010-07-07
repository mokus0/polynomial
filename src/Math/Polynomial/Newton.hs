module Math.Polynomial.Newton where

import Math.Polynomial
import Data.List

-- |Returns the Newton basis set of polynomials associated with a set of 
-- abscissas.  This is the set of monic polynomials each of which is @0@ 
-- at all previous points in the input list.
newtonBasis :: Num a => [a] -> [Poly a]
newtonBasis xs = 
    [ foldl multPoly (poly LE [1]) 
        [ poly LE [-x_i, 1]
        | x_i <- xs'
        ]
    | xs' <- inits xs
    ]
