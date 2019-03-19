{-# LANGUAGE RankNTypes, ViewPatterns #-}
module Math.Polynomial.Laurent where

import Data.List (dropWhileEnd)
import Math.Polynomial
import Math.Polynomial.Type

-- Laurent is always LE
data Laurent a = Laurent Int [a]
  deriving (Show, Eq)

newLaurent :: (Num a, Eq a) => Int -> [a] -> Laurent a
newLaurent exp coeffs
  | null coeffs' = zeroLaurent
  | otherwise = if exp >= 0
      then Laurent 0 (replicate exp 0 ++ coeffs')
      else dropWhile0 exp coeffs'
  where
    coeffs' = dropWhileEnd (== 0) coeffs
    dropWhile0 e [] = zeroLaurent
    dropWhile0 e c@(x:xs)
      = if e < 0 && x == 0
        then dropWhile0 (e + 1) xs
        else Laurent e c

expLaurent :: Laurent a -> Int
expLaurent (Laurent exp _) = exp

coeffsLaurent :: Laurent a -> [a]
coeffsLaurent (Laurent _ coeffs) = coeffs

instance (Num a, Eq a) => Num (Laurent a) where
  (+)           = addLaurent
  (*)           = multLaurent
  abs           = error ""
  signum        = error ""
  negate (Laurent ex coeffs) = Laurent ex (polyCoeffs LE (negatePoly $ poly LE coeffs))
  fromInteger a = newLaurent 0 [fromInteger a]

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
    go e [] = zeroLaurent
    go e (x:xs)
      = if e < 0 && x == 0
        then go (e + 1) xs
        else Laurent e (x:xs)

expandLaurent :: (Eq a, Num a) => Laurent a -> Laurent a
expandLaurent (Laurent exp coeffs) = newLaurent exp coeffs

evalLaurent :: (Eq a, Num a, Fractional a) => a -> Laurent a -> a
evalLaurent x (Laurent ex coeffs)
  = if ex < 0
      then evalPoly (poly BE (take (absEx) coeffs ++ zeros)) (recip x)
            + evalPoly (poly LE (drop (absEx) coeffs)) x
      else evalPoly (poly LE (replicate ex 0 ++ coeffs)) x
  where
    zeros = if length coeffs > absEx
              then [0]
              else replicate (absEx - length coeffs + 1) 0
    absEx = abs ex

laurentToPoly :: (Num a, Eq a) => Laurent a -> Poly a
laurentToPoly (Laurent exp coeffs)
  | exp >= 0 = poly LE (replicate exp 0 ++ coeffs)
  | otherwise = error "Cannot convert a laurent polynomial with negative exponents"

polyToLaurent :: (Num a, Eq a) => Poly a -> Laurent a
polyToLaurent poly = Laurent 0 (polyCoeffs LE poly)

zeroLaurent :: Laurent a
zeroLaurent = Laurent 0 []

oneLaurent :: (Num a, Eq a) => Laurent a
oneLaurent = Laurent 0 [1]

laurentIsZero :: (Eq a, Num a) => Laurent a -> Bool
laurentIsZero (Laurent exp coeffs)
  | coeffs == [] = True
  | otherwise = and (fmap (== 0) coeffs)
