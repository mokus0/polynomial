{-# LANGUAGE ViewPatterns, TypeFamilies, GADTs, UndecidableInstances #-}
-- |Low-level interface for the 'Poly' type.
module Math.Polynomial.Type
    ( Endianness(..)
    , Poly

    , zero

    , poly, polyN
    , unboxedPoly, unboxedPolyN

    , mapPoly
    , rawMapPoly
    , wrapPoly
    , unwrapPoly

    , unboxPoly

    , rawListPoly
    , rawListPolyN
    , rawVectorPoly
    , rawUVectorPoly
    , trim
    , vTrim

    , polyIsZero
    , polyIsOne

    , polyCoeffs
    , vPolyCoeffs
    , rawCoeffsOrder
    , rawPolyCoeffs
    , untrimmedPolyCoeffs

    , polyDegree
    , rawPolyDegree
    , rawPolyLength

    ) where

import Control.DeepSeq
-- import Data.List.Extras.LazyLength
import Data.AdditiveGroup
import Data.VectorSpace
import Data.VectorSpace.WrappedNum
import Data.List.ZipSum
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

-- 'unsafeCoerce' is only used in 'wrapPoly' and 'unwrapPoly', which are
-- type-safe alternatives to 'fmap'ing the 'WrappedNum' newtype constructor/projector
import Unsafe.Coerce (unsafeCoerce)

data Endianness
    = BE
    -- ^ Big-Endian (head is highest-order term)
    | LE
    -- ^ Little-Endian (head is const term)
    deriving (Eq, Ord, Enum, Bounded, Show)

instance NFData Endianness where
    rnf x = seq x ()

data Poly a where
    ListPoly ::
        { trimmed    :: !Bool
        , endianness :: !Endianness
        , listCoeffs :: ![a]
        } -> Poly a
    VectorPoly ::
        { trimmed    :: !Bool
        , endianness :: !Endianness
        , vCoeffs    :: !(V.Vector a)
        } -> Poly a
    UVectorPoly :: UV.Unbox a =>
        { trimmed    :: !Bool
        , endianness :: !Endianness
        , uvCoeffs   :: !(UV.Vector a)
        } -> Poly a

instance NFData a => NFData (Poly a) where
    rnf (ListPoly    _ _ c) = rnf c
    rnf (VectorPoly  _ _ c) = V.foldr' seq () c
    rnf (UVectorPoly _ _ _) = ()

instance Show a => Show (Poly a) where
    showsPrec p f
        = showParen (p > 10)
            ( showString "poly "
            . showsPrec 11 (rawCoeffsOrder f)
            . showChar ' '
            . showsPrec 11 (rawPolyCoeffs f)
            )

-- TODO: specialize for case where one is a list and other is a vector;
--  use native order of the list
-- TODO: think about plain Num support...
instance (AdditiveGroup a, Eq a) => Eq (Poly a) where
    p == q
        | rawCoeffsOrder p == rawCoeffsOrder q
        =  rawPolyCoeffs (trim (zeroV==) p)
        == rawPolyCoeffs (trim (zeroV==) q)
        | otherwise
        =  vPolyCoeffs LE p
        == vPolyCoeffs LE q

-- -- Ord would be nice for some purposes, but it really just doesn't
-- -- make sense (there is no natural order that is much better than any
-- -- other, AFAIK), so I'm leaving it out.
-- instance (Num a, Ord a) => Ord (Poly a) where
--     compare p q = mconcat
--             [ lengthCompare pCoeffs qCoeffs
--             , compare       pCoeffs qCoeffs
--             ]
--         where
--             pCoeffs = polyCoeffs BE p
--             qCoeffs = polyCoeffs BE q

instance Functor Poly where
    fmap f (ListPoly    _ end cs) = ListPoly   False end (map f cs)
    fmap f (VectorPoly  _ end cs) = VectorPoly False end (V.map f cs)
    -- TODO: make sure this gets fused
    fmap f (UVectorPoly _ end cs) = VectorPoly False end (V.fromListN n . map f $ UV.toList cs)
        where n = UV.length cs

-- |Like fmap, but able to preserve unboxedness
mapPoly :: (Num a, Eq a) => (a -> a) -> Poly a -> Poly a
mapPoly f = trim (0==) . rawMapPoly f

rawMapPoly :: (a -> a) -> Poly a -> Poly a
rawMapPoly f (ListPoly    _ e cs) = ListPoly    False e (   map f cs)
rawMapPoly f (VectorPoly  _ e cs) = VectorPoly  False e ( V.map f cs)
rawMapPoly f (UVectorPoly _ e cs) = UVectorPoly False e (UV.map f cs)

{-# RULES "wrapPoly/unwrapPoly"   forall x. wrapPoly (unwrapPoly x) = x #-}
{-# RULES "unwrapPoly/wrapPoly"   forall x. unwrapPoly (wrapPoly x) = x #-}
{-# RULES "wrapPoly.unwrapPoly"   wrapPoly . unwrapPoly = id #-}
{-# RULES "unwrapPoly.wrapPoly"   unwrapPoly . wrapPoly = id #-}
-- |like @fmap WrapNum@ but using 'unsafeCoerce' to avoid a pointless traversal
wrapPoly :: Poly a -> Poly (WrappedNum a)
wrapPoly = unsafeCoerce

-- |like @fmap unwrapNum@ but using 'unsafeCoerce' to avoid a pointless traversal
unwrapPoly :: Poly (WrappedNum a) -> Poly a
unwrapPoly = unsafeCoerce

instance AdditiveGroup a => AdditiveGroup (Poly a) where
    zeroV = ListPoly True LE []
    (untrimmedPolyCoeffs LE ->  a) ^+^ (untrimmedPolyCoeffs LE ->  b)
        = ListPoly False LE (zipSumV a b)
    negateV = fmap negateV

instance (Eq a, VectorSpace a, AdditiveGroup (Scalar a), Eq (Scalar a)) => VectorSpace (Poly a) where
    type Scalar (Poly a) = Scalar a
    s *^ v
         | s == zeroV   = zeroV
         | otherwise    = vTrim (rawMapPoly (s *^) v)

-- |Trim zeroes from a polynomial (given a predicate for identifying zero).
-- In particular, drops zeroes from the highest-order coefficients, so that
-- @0x^n + 0x^(n-1) + 0x^(n-2) + ... + ax^k + ...@, @a /= 0@
-- is normalized to @ax^k + ...@.
--
-- The 'Eq' instance for 'Poly' and all the standard constructors / destructors
-- are defined using @trim (0==)@.
trim :: (a -> Bool) -> Poly a -> Poly a
trim _ p | trimmed p = p
trim isZero   (ListPoly    _ LE cs) = ListPoly    True LE (dropEnd   isZero cs)
trim isZero   (ListPoly    _ BE cs) = ListPoly    True BE (dropWhile isZero cs)
trim isZero   (VectorPoly  _ LE cs) = VectorPoly  True LE (V.reverse . V.dropWhile isZero . V.reverse $ cs)
trim isZero   (VectorPoly  _ BE cs) = VectorPoly  True BE (V.dropWhile isZero cs)
trim isZero   (UVectorPoly _ LE cs) = UVectorPoly True LE (UV.reverse . UV.dropWhile isZero . UV.reverse $ cs)
trim isZero   (UVectorPoly _ BE cs) = UVectorPoly True BE (UV.dropWhile isZero cs)

vTrim :: (Eq a, AdditiveGroup a) => Poly a -> Poly a
vTrim = trim (zeroV ==)

-- |The polynomial \"0\"
zero :: Poly a
zero = ListPoly True LE []

-- |Make a 'Poly' from a list of coefficients using the specified coefficient order.
poly :: (Num a, Eq a) => Endianness -> [a] -> Poly a
poly end = trim (0==) . rawListPoly end

-- |Make a 'Poly' from a list of coefficients, at most 'n' of which are significant.
polyN :: (Num a, Eq a) => Int -> Endianness -> [a] -> Poly a
polyN n end = trim (0==) . rawVectorPoly end . V.fromListN n

unboxedPoly :: (UV.Unbox a, Num a, Eq a) => Endianness -> [a] -> Poly a
unboxedPoly end = trim (0==) . rawUVectorPoly end . UV.fromList

unboxedPolyN :: (UV.Unbox a, Num a, Eq a) => Int -> Endianness -> [a] -> Poly a
unboxedPolyN n end = trim (0==) . rawUVectorPoly end . UV.fromListN n

unboxPoly :: UV.Unbox a => Poly a -> Poly a
unboxPoly (ListPoly   t e cs) = UVectorPoly t e (UV.fromList cs)
unboxPoly (VectorPoly t e cs) = UVectorPoly t e (UV.fromListN (V.length cs) (V.toList cs))
unboxPoly p@UVectorPoly{} = p

-- |Make a 'Poly' from a list of coefficients using the specified coefficient order,
-- without the 'Num' context (and therefore without trimming zeroes from the
-- coefficient list)
rawListPoly :: Endianness -> [a] -> Poly a
rawListPoly = ListPoly False

rawListPolyN :: Int -> Endianness -> [a] -> Poly a
rawListPolyN n e = rawVectorPoly e . V.fromListN n

rawVectorPoly :: Endianness -> V.Vector a -> Poly a
rawVectorPoly = VectorPoly False

rawUVectorPoly :: UV.Unbox a => Endianness -> UV.Vector a -> Poly a
rawUVectorPoly = UVectorPoly False

-- |Get the degree of a a 'Poly' (the highest exponent with nonzero coefficient)
polyDegree :: (Num a, Eq a) => Poly a -> Int
polyDegree p = rawPolyDegree (trim (0==) p)

rawPolyDegree :: Poly a -> Int
rawPolyDegree p = rawPolyLength p - 1

rawPolyLength :: Poly a -> Int
rawPolyLength (ListPoly    _ _ cs) =    length cs
rawPolyLength (VectorPoly  _ _ cs) =  V.length cs
rawPolyLength (UVectorPoly _ _ cs) = UV.length cs


-- |Get the coefficients of a a 'Poly' in the specified order.
polyCoeffs :: (Num a, Eq a) => Endianness -> Poly a -> [a]
polyCoeffs end p = untrimmedPolyCoeffs end (trim (0 ==) p)

-- |Get the coefficients of a a 'Poly' in the specified order.
vPolyCoeffs :: (Eq a, AdditiveGroup a) => Endianness -> Poly a -> [a]
vPolyCoeffs end p = untrimmedPolyCoeffs end (vTrim p)

polyIsZero :: (Num a, Eq a) => Poly a -> Bool
polyIsZero = null . rawPolyCoeffs . trim (0==)

polyIsOne :: (Num a, Eq a) => Poly a -> Bool
polyIsOne = ([1]==) . rawPolyCoeffs . trim (0==)

rawCoeffsOrder :: Poly a -> Endianness
rawCoeffsOrder = endianness

rawPolyCoeffs :: Poly a -> [a]
rawPolyCoeffs p@ListPoly{}         = listCoeffs p
rawPolyCoeffs p@VectorPoly{}       = V.toList (vCoeffs p)
rawPolyCoeffs p@UVectorPoly{}      = UV.toList (uvCoeffs p)

-- TODO: make sure (V.toList . V.reverse) gets fused
untrimmedPolyCoeffs :: Endianness -> Poly a -> [a]
untrimmedPolyCoeffs e1 (VectorPoly  _ e2 cs)
    | e1 == e2  = V.toList cs
    | otherwise = V.toList  (V.reverse cs)
untrimmedPolyCoeffs e1 (UVectorPoly _ e2 cs)
    | e1 == e2  = UV.toList cs
    | otherwise = UV.toList (UV.reverse cs)
untrimmedPolyCoeffs e1 (ListPoly _ e2 cs)
    | e1 == e2  = cs
    | otherwise = reverse cs

dropEnd :: (a -> Bool) -> [a] -> [a]
-- dropEnd p = reverse . dropWhile p . reverse
dropEnd p = go id
    where
        go t (x:xs)
            -- if p x, stash x (will only be used if 'not (any p xs)')
            | p x       =        go (t.(x:))  xs
            -- otherwise insert x and all stashed values in output and reset the stash
            | otherwise = t (x : go  id       xs)
        -- at end of string discard the stash
        go _ [] = []
