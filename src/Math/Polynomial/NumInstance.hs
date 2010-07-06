{-# OPTIONS_GHC -fno-warn-orphans #-}
module Math.Polynomial.NumInstance where

-- |This module exports a 'Num' instance for the 'Poly' type.
-- This instance does not implement all operations, because 'abs' and 'signum'
-- are simply not definable, so I have placed it into a separate module so
-- that I can make people read this caveat ;).
--
-- Use at your own risk.
import Math.Polynomial

instance Num a => Num (Poly a) where
    fromInteger i = poly LE [fromInteger i]
    (+) = addPoly
    negate = fmap negate
    (*) = multPoly

    abs     = error    "abs cannot be defined for the Poly type"
    signum  = error "signum cannot be defined for the Poly type"
