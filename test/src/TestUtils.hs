{-# LANGUAGE TypeSynonymInstances, TypeFamilies #-}
module TestUtils where

import Control.Applicative
import Data.List
import Data.VectorSpace
import Math.Polynomial
import Test.QuickCheck

instance Arbitrary Endianness where
    arbitrary = elements [BE, LE]

instance (Num a, Arbitrary a) => Arbitrary (Poly a) where
    arbitrary = poly <$> arbitrary <*> arbitrary

instance AdditiveGroup Rational where
    zeroV = 0
    (^+^) = (+)
    negateV = negate
instance VectorSpace Rational where
    type Scalar Rational = Rational
    (*^) = (*)

rev BE = LE
rev LE = BE

sep [] = []
sep rts = uniq : sep (rts \\ uniq)
    where uniq = nub rts

-- arbitrary function mapping the real line to the unit interval
onUnitInterval x = x - fromIntegral (floor x)

onInterval a b x = a + w * onUnitInterval x
    where
        w = b - a

distinct [] = True
distinct (x:xs) = all (/= x) xs && distinct xs

relErr 0 0 = 0
relErr x y = abs (x - y) / max (abs x) (abs y)
