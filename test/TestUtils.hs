{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, TypeFamilies #-}
module TestUtils where

import Control.Applicative
import Control.Monad
import Data.List
import Data.VectorSpace
import Math.Polynomial
import Math.Polynomial.Type
import Math.Polynomial.Laurent
import Test.QuickCheck
import qualified Data.Vector as V

instance Arbitrary Endianness where
    arbitrary = elements [BE, LE]

instance (Num a, Eq a, Arbitrary a) => Arbitrary (Poly a) where
    arbitrary = polyCon <*> arbitrary <*> arbitrary
        where
            polyCon = elements
                [ poly
                , rawListPoly
                , \e -> rawVectorPoly e . V.fromList
                ]

instance (Num a, Eq a, Arbitrary a) => Arbitrary (Laurent a) where
    arbitrary = newLaurent <$> arbitrary <*> arbitrary

newtype SmallPoly a = SmallPoly (Poly a)
    deriving (Eq, Show)
instance (Num a, Eq a, Arbitrary a) => Arbitrary (SmallPoly a) where
    arbitrary = SmallPoly <$> (polyCon <*> arbitrary <*> smallList)
        where
            polyCon = elements
                [ poly
                , rawListPoly
                , \e -> rawVectorPoly e . V.fromList
                ]

            smallList = do
                let binom n k = product [k+1 .. n] `div` product [1 .. n-k]
                n <- frequency [ (fromInteger (binom 10 k), return k) | k <- [0..10]]
                replicateM (fromInteger n) arbitrary

-- instance AdditiveGroup Rational where
--     zeroV = 0
--     (^+^) = (+)
--     negateV = negate
-- instance VectorSpace Rational where
--     type Scalar Rational = Rational
--     (*^) = (*)

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
