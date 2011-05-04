module Math.Polynomial.Hermite where

import Math.Polynomial
import Data.VectorSpace

probHermite :: [Poly Integer]
probHermite 
    = one
    : [ multPoly x h_n ^-^ polyDeriv h_n
      | h_n <- probHermite
      ]

physHermite :: [Poly Integer]
physHermite
    = one
    : [ scalePoly 2 (multPoly x h_n) ^-^ polyDeriv h_n
      | h_n <- physHermite
      ]

evalProbHermite :: (Integral a, Num b) => a -> b -> b
evalProbHermite n = fst . evalProbHermiteDeriv n

evalProbHermiteDeriv :: (Integral a, Num b) => a -> b -> (b, b)
evalProbHermiteDeriv 0 _ = (1, 0)
evalProbHermiteDeriv 1 x = (x, 1)
evalProbHermiteDeriv n x
    | n < 0     = error "evalProbHermite: n < 0"
    | otherwise = loop 1 x 1
    where
        loop k h_k h_km1
            | k == n    = (h_k, k' * h_km1)
            | otherwise = loop (k+1) (x * h_k - k' * h_km1) h_k
            where k' = fromIntegral k

evalPhysHermite :: (Integral a, Num b) => a -> b -> b
evalPhysHermite n = fst . evalPhysHermiteDeriv n

evalPhysHermiteDeriv :: (Integral a, Num b) => a -> b -> (b,b)
evalPhysHermiteDeriv 0 _ = (1,   0)
evalPhysHermiteDeriv 1 x = (2*x, 2)
evalPhysHermiteDeriv n x
    | n < 0     = error "evalProbHermite: n < 0"
    | otherwise = loop 1 (2*x) 1
    where
        loop k h_k h_km1
            | k == n    = (h_k, 2 * k' * h_km1)
            | otherwise = loop (k+1) (2 * (x * h_k - k' * h_km1)) h_k
            where k' = fromIntegral k
