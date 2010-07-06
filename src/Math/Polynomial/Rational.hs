{-# LANGUAGE ViewPatterns #-}
-- |This stuff is somewhat experimental.  I am not sure how useful the 
-- rational function manipulation stuff is, overall, due to truncation
-- error issues in polynomial division.  Works well with exact Rational
-- coefficients though, and the evaluation functions are fine for any
-- Fractional type.
module Math.Polynomial.Rational where

import Math.Polynomial
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

data RationalPoly a = RationalPoly
    { reduced :: !Bool
    , numerator :: Poly a
    , denominator :: Poly a
    }

instance Fractional a => Show (RationalPoly a) where
    showsPrec prec (reduce -> RationalPoly _ p q) = showParen (prec > 7)
        ( showsPrec 7 p
        . showString " % " 
        . showsPrec 8 q
        )

instance Functor RationalPoly where
    fmap f (RationalPoly _ p q) = RationalPoly False (fmap f p) (fmap f q)

reduce r
    | reduced r = r
    | polyIsZero q  = error "%: divide by zero"
    | otherwise = RationalPoly True (p `quotPoly` elim) (q `quotPoly` elim)
        where
            p = numerator r
            q = denominator r
            q0 = head (polyCoeffs BE q)
            elim = fmap (/ q0) (gcdPoly p q)

infixl %
p % q = reduce (RationalPoly False p q)

instance (Fractional a, Pretty a, Ord a) => Pretty (RationalPoly a) where
    pPrintPrec l prec (reduce -> RationalPoly _ p q) = prettyParen (prec > 7) $ sep
        [ pPrintPrec l 7 p
        , text "/"
        , pPrintPrec l 8 q
        ]

addRational (reduce -> RationalPoly _ xp xq) (reduce -> RationalPoly _ yp yq)
    = num % den
    where
        num = addPoly
            (multPoly xp (quotPoly xq cd))
            (multPoly yp (quotPoly yq cd))
        den = multPoly xq yq `quotPoly` cd
        cd = gcdPoly xq yq

negateRational r = r {numerator = negatePoly (numerator r)}
scaleRational s r = r {numerator = scalePoly s (numerator r)}

multRational (reduce -> RationalPoly _ xp xq) (reduce -> RationalPoly _ yp yq)
    = multPoly xp yp % multPoly xq yq

recipRational (reduce -> RationalPoly _ p q) = q % p

divRational (reduce -> RationalPoly _ xp xq) (reduce -> RationalPoly _ yp yq)
    = multPoly xp yq % multPoly xq yp

evalRational (reduce -> RationalPoly _ p q) x = evalPoly p x / evalPoly q x

rationalDeriv (reduce -> RationalPoly _ g h) = (g' * h - g * h') % (h*h)
    where
        (*) = multPoly
        (-) = \x y -> addPoly x (negatePoly y)
        g' = polyDeriv g
        h' = polyDeriv h