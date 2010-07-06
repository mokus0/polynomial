{-# LANGUAGE ParallelListComp #-}
module Math.Polynomial.Chebyshev where

import Math.Polynomial
import Data.List

-- |The Chebyshev polynomials of the first kind with 'Integer' coefficients.
ts :: [Poly Integer]
ts = poly LE [1] : 
    [ addPoly (poly LE [0, 1]    `multPoly` t_n)
              (poly LE [-1,0,1] `multPoly` u_n)
    | t_n <- ts
    | u_n <- poly LE [0] : us
    ]

-- The Chebyshev polynomials of the second kind with 'Integer' coefficients.
us :: [Poly Integer]
us = 
    [ addPoly t_n (multPoly u_n (poly LE [0,1]))
    | t_n <- ts
    | u_n <- poly LE [0] : us
    ]

-- |Compute the coefficients of the n'th Chebyshev polynomial of the first kind.
t :: Num a => Int -> Poly a
t n = poly LE . map fromInteger . polyCoeffs LE $ ts !! n

-- |Compute the coefficients of the n'th Chebyshev polynomial of the second kind.
u :: Num a => Int -> Poly a
u n = poly LE . map fromInteger . polyCoeffs LE $ us !! n

-- |Evaluate the n'th Chebyshev polynomial of the first kind at a point X.  
-- Both more efficient and more numerically stable than computing the 
-- coefficients and evaluating the polynomial.
evalT :: Num a => Int -> a -> a
evalT n x = evalTs x !! n

-- |Evaluate all the Chebyshev polynomials of the first kind at a point X.
evalTs :: Num a => a -> [a]
evalTs = fst . evalTsUs

-- |Evaluate the n'th Chebyshev polynomial of the second kind at a point X.  
-- Both more efficient and more numerically stable than computing the 
-- coefficients and evaluating the polynomial.
evalU :: Num a => Int -> a -> a
evalU n x = evalUs x !! n

-- |Evaluate all the Chebyshev polynomials of the second kind at a point X.
evalUs :: Num a => a -> [a]
evalUs = snd . evalTsUs

-- |Evaluate the n'th Chebyshev polynomials of both kinds at a point X.
evalTU :: Num a => Int -> a -> (a,a)
evalTU n x = (ts!!n, us!!n)
    where (ts,us) = evalTsUs x

-- |Evaluate all the Chebyshev polynomials of the both kinds at a point X.
evalTsUs :: Num a => a -> ([a], [a])
evalTsUs x = (ts, tail us)
    where
        ts = 1 : [x * t_n - (1-x*x)*u_n  | t_n <- ts | u_n <- us]
        us = 0 : [x * u_n + t_n          | t_n <- ts | u_n <- us]

-- |Compute the roots of the n'th Chebyshev polynomial of the first kind.
tRoots :: Floating a => Int -> [a]
tRoots   n = [cos (pi / fromIntegral n * (fromIntegral k + 0.5)) | k <- [0..n-1]]

-- |Compute the extreme points of the n'th Chebyshev polynomial of the first kind.
tExtrema :: Floating a => Int -> [a]
tExtrema n = [cos (pi / fromIntegral n *  fromIntegral k       ) | k <- [0..n]]

-- |@chebyshevFit n f@ returns a list of N coefficients @cs@ such that 
-- @f x@ ~= @sum (zipWith (*) cs (evalTs x))@ on the interval -1 < x < 1.
-- 
-- The N roots of the N'th Chebyshev polynomial are the fitting points at 
-- which the function will be evaluated and at which the approximation will be
-- exact.  These points always lie within the interval -1 < x < 1.  Outside
-- this interval, the approximation will diverge quickly.
--
-- This function deviates from most chebyshev-fit implementations in that it
-- returns the first coefficient pre-scaled so that the series evaluation 
-- operation is a simple inner product, since in most other algorithms
-- operating on chebyshev series, that factor is almost always a nuissance.
chebyshevFit :: Floating a => Int -> (a -> a) -> [a]
chebyshevFit n f = 
    [ oneOrTwo / fromIntegral n 
    * sum (zipWith (*) ts fxs)
    | ts <- transpose txs
    | oneOrTwo <- 1 : repeat 2
    ]
    where
        txs = map (take n . evalTs) xs
        fxs = map f xs
        xs = tRoots n

-- |Evaluate a Chebyshev series expansion with a finite number of terms.
-- 
-- Note that this function expects the first coefficient to be pre-scaled
-- by 1/2, which is what is produced by 'chebyshevFit'.  Thus, this computes
-- a simple inner product of the given list with a matching-length sequence of
-- chebyshev polynomials.
evalChebyshevSeries :: Num a => [a] -> a -> a
evalChebyshevSeries     []  _ = 0
evalChebyshevSeries (c0:cs) x = 
        let b1:b2:_ = reverse bs
         in x*b1 - b2 + c0
    where
        -- Clenshaw's recurrence formula
        bs = 0 : 0 : [2*x*b1 - b2 + c | b2:b1:_ <- tails bs | c <- reverse cs]
