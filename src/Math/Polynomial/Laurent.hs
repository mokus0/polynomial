{-# LANGUAGE RankNTypes, ViewPatterns #-}
module Math.Polynomial.Laurent where

import Data.List (dropWhileEnd)
import Math.Polynomial

data Laurent a = Laurent Int [a]
  deriving (Show, Eq)

-- |Make a `Laurent` polynomial from an degree
-- and a list of coefficients using the LE order.
newLaurent :: (Num a, Eq a) => Int -> [a] -> Laurent a
newLaurent exp coeffs
  | null coeffs' = zeroLaurent
  | otherwise = if exp >= 0
      then Laurent 0 (replicate exp 0 ++ coeffs')
      else dropWhile0 exp coeffs'
  where
    coeffs' = dropWhileEnd (== 0) coeffs
    dropWhile0 _ [] = zeroLaurent
    dropWhile0 e c@(x:xs)
      = if e < 0 && x == 0
        then dropWhile0 (e + 1) xs
        else Laurent e c

expLaurent :: Laurent a -> Int
expLaurent (Laurent exp _) = exp

coeffsLaurent :: Laurent a -> [a]
coeffsLaurent (Laurent _ coeffs) = coeffs

-- |Given some laurent polynomial 'f', computes the polynomial 'g' such that:
--
-- > evalLaurent g x = negate (evalLaurent f x)
negateLaurent :: (Num a, Eq a) => Laurent a -> Laurent a
negateLaurent (Laurent ex coeffs) = Laurent ex (polyCoeffs LE (negatePoly $ poly LE coeffs))

-- |Given laurent polynomials 'f' and 'g', computes the polynomial 'h' such that:
--
-- > evalLaurent h x = evalLaurent f x + evalLaurent g x
addLaurent :: forall a. (Eq a, Num a) => Laurent a -> Laurent a -> Laurent a
addLaurent (Laurent _ []) y  = y
addLaurent  x          (Laurent _ []) = x
addLaurent (Laurent xt x) (Laurent yt y) =
   if xt > yt
     then newLaurent yt (addShifted (xt - yt) y x)
     else newLaurent xt (addShifted (yt - xt) x y)
  where
    addShifted :: (Eq a, Num a) => Int -> [a] -> [a] -> [a]
    addShifted del p1 p2
      = if del >= 0
          then polyCoeffs LE (poly LE (replicate del 0 ++ p2) `addPoly` poly LE p1)
          else error "Laurent addShifted: negative shift"

-- |Given laurent polynomials 'f' and 'g', computes the polynomial 'h' such that:
--
-- > evalLaurent h x = evalLaurent f x * evalLaurent g x
multLaurent :: (Eq a, Num a) => Laurent a -> Laurent a -> Laurent a
multLaurent (Laurent expA coeffsA) (Laurent expB coeffsB)
  = newLaurent (expA + expB) (polyCoeffs LE $ poly LE coeffsA `multPoly` poly LE coeffsB)

quotLaurent :: (Eq a, Num a, Fractional a) => Laurent a -> Laurent a -> Laurent a
quotLaurent (reduceLaurent -> Laurent expA coeffsA) (reduceLaurent -> Laurent expB coeffsB)
  = if (lengthDiff < 0)
    then newLaurent
          (expA - expB + lengthDiff)
          (polyCoeffs LE $ poly LE (replicate (abs lengthDiff) 0 ++ coeffsA) `quotPoly` poly LE coeffsB)
    else newLaurent
          (expA - expB)
          (polyCoeffs LE $ poly LE coeffsA `quotPoly` poly LE coeffsB)
  where
    lengthDiff = length coeffsA - length coeffsB

-- |Given laurent polynomials @a@ and @b@, with @b@ not 'zero', computes polynomials
-- @q@ and @r@ such that:
--
-- > addLaurent (multLaurent q b) r == a
quotRemLaurent :: (Eq a, Num a, Fractional a) => Laurent a -> Laurent a -> (Laurent a, Laurent a)
quotRemLaurent (reduceLaurent -> Laurent expA coeffsA) (reduceLaurent -> Laurent expB coeffsB)
  = if lengthDiff < 0
    then let (q, r) = poly LE (replicate (abs lengthDiff) 0 ++ coeffsA) `quotRemPoly` poly LE coeffsB
         in ( newLaurent (expA - expB + lengthDiff) (polyCoeffs LE q)
            , newLaurent (expA + lengthDiff) (polyCoeffs LE r)
            )
    else let (q, r) = poly LE coeffsA `quotRemPoly` poly LE coeffsB
         in ( newLaurent (expA - expB) (polyCoeffs LE q)
            , newLaurent expA (polyCoeffs LE r)
            )
  where
    lengthDiff = length coeffsA - length coeffsB

reduceLaurent :: (Eq a, Num a) => Laurent a -> Laurent a
reduceLaurent (Laurent exp coeffs) = go exp coeffs
  where
    go _ [] = zeroLaurent
    go e (x:xs)
      = if e < 0 && x == 0
        then go (e + 1) xs
        else Laurent e (x:xs)

expandLaurent :: (Eq a, Num a) => Laurent a -> Laurent a
expandLaurent (Laurent exp coeffs) = newLaurent exp coeffs

-- |Evaluate a laurent polynomial at a point or, equivalently, convert a polynomial
-- to the function it represents.  For example, @evalLaurent 'x' = 'id'@
evalLaurent :: (Eq a, Num a, Fractional a) => Laurent a -> a -> a
evalLaurent (Laurent ex coeffs) x
  = if ex < 0
      then evalPoly (poly BE (take (absEx) coeffs ++ zeros)) (recip x)
            + evalPoly (poly LE (drop (absEx) coeffs)) x
      else evalPoly (poly LE (replicate ex 0 ++ coeffs)) x
  where
    zeros = if length coeffs > absEx
              then [0]
              else replicate (absEx - length coeffs + 1) 0
    absEx = abs ex

-- |Convert a polynomial of type Laurent to a polynomial of type Poly
laurentToPoly :: (Num a, Eq a) => Laurent a -> Maybe (Poly a)
laurentToPoly (Laurent exp coeffs)
  | exp >= 0 = Just $ poly LE (replicate exp 0 ++ coeffs)
  | otherwise = Nothing

-- |Convert a polynomial of type Poly to a laurent polynomial of type Laurent
polyToLaurent :: (Num a, Eq a) => Poly a -> Laurent a
polyToLaurent poly = Laurent 0 (polyCoeffs LE poly)

-- |The polynomial \"0\"
zeroLaurent :: Laurent a
zeroLaurent = Laurent 0 []

-- |The polynomial \"0\"
oneLaurent :: (Num a, Eq a) => Laurent a
oneLaurent = Laurent 0 [1]

laurentIsZero :: (Eq a, Num a) => Laurent a -> Bool
laurentIsZero (Laurent _ coeffs)
  | coeffs == [] = True
  | otherwise = and (fmap (== 0) coeffs)

instance (Num a, Eq a) => Num (Laurent a) where
  (+)           = addLaurent
  (*)           = multLaurent
  negate        = negateLaurent
  fromInteger a = newLaurent 0 [fromInteger a]

  abs     = error    "abs cannot be defined for the Laurent type"
  signum  = error "signum cannot be defined for the Laurent type"
