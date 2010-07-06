{-# LANGUAGE ParallelListComp #-}
module Math.Polynomial.Bernstein where

import Math.Polynomial
import Data.List
import Data.Function

-- |The Bernstein basis polynomials.  Each inner list is a basis.  The outer
-- list is a list of bases in ascending polynomial order.
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

evalBernstein :: (Integral a, Fractional b) => a -> a -> b -> b
evalBernstein n v t = nCv * t^^v * (1-t)^^(n-v)
    where
        nCv = product (zipWith ((/) `on` fromIntegral) [1..n] ([1..v] ++ [1..n-v]))

bernsteinFit :: (Fractional b, Integral a) => a -> (b -> b) -> [b]
bernsteinFit n f = [f (fromIntegral v / fromIntegral n) | v <- [0..n]]

evalBernsteinSeries :: Num a => [a] -> a -> a
evalBernsteinSeries [] = const 0
evalBernsteinSeries cs = head . last . deCasteljau cs

-- |de Casteljau's algorithm, returning the whole tableau.  Used both for
-- evaluating and splitting polynomials in Bernstein form.
deCasteljau :: Num a => [a] -> a -> [[a]]
deCasteljau cs t = takeWhile (not.null) table
    where
        table = cs : 
            [ [ b_i * (1-t) + b_ip1 * t
              | b_i:b_ip1:_ <- tails row
              ]
            | row <- table
            ]

-- |Given a polynomial in Bernstein form and a parameter value @x@, split the
-- polynomial into two halves, mapping @[0,x]@ and @[x,1]@ respectively 
-- onto @[0,1]@.
splitBernsteinSeries :: Num a => [a] -> a -> ([a], [a])
splitBernsteinSeries cs t = (map head betas, map last (reverse betas))
    where
        betas = deCasteljau cs t
