module Math.Polynomial.Newton where

import Math.Polynomial
import Data.List

newtonBasis :: Num a => [a] -> [Poly a]
newtonBasis xs = 
    [ foldl multPoly (poly LE [1]) 
        [ poly LE [-x_i, 1]
        | x_i <- xs'
        ]
    | xs' <- inits xs
    ]
