{-# LANGUAGE ParallelListComp #-}
module Math.Polynomial.Legendre where

import Math.Polynomial

-- |The Legendre polynomials with 'Rational' coefficients.
legendres :: [Poly Rational]
legendres = one : x : 
    [ multPoly
        (poly LE [recip (n' + 1)])
        (addPoly (poly LE [0, 2 * n' + 1] `multPoly` p_n)
                 (poly LE           [-n'] `multPoly` p_nm1)
        )
    | n     <- [1..], let n' = fromInteger n
    | p_n   <- tail legendres
    | p_nm1 <- legendres
    ]

-- |Compute the coefficients of the n'th Legendre polynomial.
legendre :: Fractional a => Int -> Poly a
legendre n = poly LE . map fromRational . polyCoeffs LE $ legendres !! n

-- |Evaluate the n'th Legendre polynomial at a point X.  Both more efficient
-- and more numerically stable than computing the coefficients and evaluating
-- the polynomial.
evalLegendre :: Fractional a => Int -> a -> a
evalLegendre n t = evalLegendres t !! n

-- |Evaluate all the Legendre polynomials at a point X.
evalLegendres :: Fractional a => a -> [a]
evalLegendres t = ps
    where
       ps = 1 : t : 
            [ ((2 * n + 1) * t * p_n - n * p_nm1) / (n + 1)
            | n     <- iterate (1+) 1
            | p_n   <- tail ps
            | p_nm1 <- ps
            ]

-- |Evaluate the n'th Legendre polynomial and its derivative at a point X.  
-- Both more efficient and more numerically stable than computing the
-- coefficients and evaluating the polynomial.
evalLegendreDeriv :: Fractional a => Int -> a -> (a,a)
evalLegendreDeriv 0 _ = (1,0)
evalLegendreDeriv n t = case drop (n-1) (evalLegendres t) of
    (p2:p1:_)   -> (p1, fromIntegral n * (t * p1 - p2) / (t*t - 1))
    _ -> error "evalLegendreDeriv: evalLegendres didn't return a long enough list" {- should be infinite -}

-- |Zeroes of the n'th Legendre polynomial.
legendreRoots :: (Fractional b, Ord b) => Int -> b -> [b]
legendreRoots n eps = map negate mRoots ++ reverse (take (n-m) mRoots)
    where
        -- the roots are symmetric in the interval so we only have to find 'm' of them.
        -- The rest are reflections.
        m = (n + 1) `div` 2
        mRoots = [improveRoot (z0 i) | i <- [0..m-1]]
        
        -- Initial guess for i'th root of the n'th Legendre polynomial
        z0 i = realToFrac (cos (pi * (fromIntegral i + 0.75) / (fromIntegral n + 0.5)) :: Double)
        -- Improve estimate of a root by newton's method
        improveRoot z1
            | abs (z2-z1) <= eps    = z2
            | otherwise             = improveRoot z2
            where
                (y, dy) = evalLegendreDeriv n z1
                z2 = z1 - y/dy
