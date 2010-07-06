{-# LANGUAGE 
        ParallelListComp, ViewPatterns,
        FlexibleInstances, FlexibleContexts, IncoherentInstances
  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- This code is a big ugly mess, but it more or less works.  Someday I might
-- get around to cleaning it up.
module Math.Polynomial.Pretty () where

import Math.Polynomial.Type

import Data.Complex

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

instance (Pretty a, Num a, Ord a) => Pretty (Poly a) where
    pPrintPrec l p x = ppr
        where
            ppr    = pPrintPolyWith p BE (pPrintOrdTerm pPrNum 'x') x
            pPrNum = pPrintPrec l 11

instance (RealFloat a, Pretty a) => Pretty (Complex a) where
    pPrintPrec l p (a :+ b) = ppr
        where
            x = poly LE [a,b]
            ppr = pPrintPolyWith p LE (pPrintOrdTerm pPrNum 'i') x
            pPrNum = pPrintPrec l 11

instance (RealFloat a, Pretty (Complex a)) => Pretty (Poly (Complex a)) where
    pPrintPrec l p x = ppr
        where
            ppr    = pPrintPolyWith p BE (pPrintUnOrdTerm pPrNum 'x') x
            pPrNum = pPrintPrec l 11

pPrintPolyWith prec end v p = parenSep (prec > 5) $ filter (not . isEmpty)
    [ v first coeff exp
    | (coeff, exp) <- 
        (if end == BE then reverse else dropWhile ((0==).fst))
        (zip (polyCoeffs LE p) [0..])
    | first <- True : repeat False
    ]

parenSep p xs = 
    prettyParen (p && not (null (drop 1 xs)))   
        (hsep xs)

pPrintOrdTerm   _ _ _ 0 _ = empty
pPrintOrdTerm num _ f c 0 = sign f c <> num (abs c)
pPrintOrdTerm   _ v f c 1   | abs c == 1    = sign f c <> char v
pPrintOrdTerm num v f c 1 = sign f c <> num (abs c) <> char v
pPrintOrdTerm   _ v f c e   | abs c == 1    = sign f c <> char v <> text "^" <> int e
pPrintOrdTerm num v f c e = sign f c <> num (abs c) <> char v <> text "^" <> int e

sign True x
    | x < 0     = char '-'
    | otherwise = empty
sign False x
    | x < 0     = text "- "
    | otherwise = text "+ "

pPrintUnOrdTerm   _ _ _ 0 _ = empty
pPrintUnOrdTerm num _ f c 0 = sign f 1 <> num c
pPrintUnOrdTerm   _ v f 1 1 = sign f 1 <> char v
pPrintUnOrdTerm num v f c 1 = sign f 1 <> num c <> char v
pPrintUnOrdTerm   _ v f 1 e = sign f 1 <> char v <> text "^" <> int e
pPrintUnOrdTerm num v f c e = sign f 1 <> num c <> char v <> text "^" <> int e

