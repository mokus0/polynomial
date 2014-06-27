module Math.Polynomial.Bernoulli (bernoulliPoly) where

import Math.Polynomial
import Data.VectorSpace

{- | Bernoulli polynomial with a nonstandard normalization

> b_i = bernoulliPoly !! i

Has the following generating function (C.2 in IH Sloan & S Joe
"Lattice Methods for multiple integration" 1994 page 227)

> t exp(x*t) / (exp(t) - 1) = sum_{i=0} b_i t^i

The standard normalization would have @= sum_{i=0} B_i t^i / i!@

-}
bernoulliPoly :: (Fractional a, Eq a) => [Poly a]
bernoulliPoly = map fst biIntegralBi

biIntegralBi :: (Fractional a, Eq a) => [(Poly a, Poly a)]
biIntegralBi = (constPoly 1, polyIntegral (constPoly 1)) : map f biIntegralBi
  where f (p, ip) = case polyIntegral ip of
                      ip2 -> case constPoly $ evalPoly ip2 0 - evalPoly ip2 1 of
                               c -> (c `addPoly` ip, polyIntegral c `addPoly` ip2)
