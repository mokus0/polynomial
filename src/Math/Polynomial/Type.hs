{-# LANGUAGE ViewPatterns, TypeFamilies #-}
-- |Low-level interface for the 'Poly' type.
module Math.Polynomial.Type 
    ( Endianness(..)
    , Poly, poly, polyCoeffs
    , trim, rawPoly, rawPolyCoeffs
    , polyIsZero, polyIsOne
    ) where

-- import Data.List.Extras.LazyLength
import Data.AdditiveGroup
import Data.VectorSpace
import Data.List.ZipSum

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

-- |Trim zeroes from a polynomial (given a predicate for identifying zero).
-- In particular, drops zeroes from the highest-order coefficients, so that
-- @0x^n + 0x^(n-1) + 0x^(n-2) + ... + ax^k + ...@, @a /= 0@
-- is normalized to @ax^k + ...@.  
-- 
-- The 'Eq' instance for 'Poly' and all the standard constructors / destructors
-- are defined using @trim (0==)@.
trim :: (a -> Bool) -> Poly a -> Poly a
trim      _ p@(Poly _ True _) = p
trim isZero   (Poly LE _ cs) = Poly LE True (dropEnd   isZero cs)
trim isZero   (Poly BE _ cs) = Poly BE True (dropWhile isZero cs)

-- |Make a 'Poly' from a list of coefficients using the specified coefficient order.
poly :: Num a => Endianness -> [a] -> Poly a
poly end cs = trim (0==) (rawPoly end cs)

-- |Make a 'Poly' from a list of coefficients using the specified coefficient order,
-- without the 'Num' context (and therefore without trimming zeroes from the 
-- coefficient list)
rawPoly :: Endianness -> [a] -> Poly a
rawPoly end cs = Poly end False cs 

-- |Get the coefficients of a a 'Poly' in the specified order.
polyCoeffs :: Num a => Endianness -> Poly a -> [a]
polyCoeffs end p = rawPolyCoeffs end (trim (0==) p)

-- |Get the coefficients of a a 'Poly' in the specified order, without the 'Num'
-- constraint (and therefore without trimming zeroes).
-- 
-- This function does not respect the 'Eq' instance:
--   @x == y@ =/=> @rawPolyCoeffs e x == rawPolyCoeffs e y@.
rawPolyCoeffs :: Endianness -> Poly a -> [a]
rawPolyCoeffs end (Poly e _ cs)
    | e == end  = cs
    | otherwise = reverse cs

polyIsZero :: Num a => Poly a -> Bool
polyIsZero = null . coeffs . trim (0==)

polyIsOne :: Num a => Poly a -> Bool
polyIsOne = ([1]==) . coeffs . trim (0==)

data Endianness 
    = BE 
    -- ^ Big-Endian (head is highest-order term)
    | LE
    -- ^ Little-Endian (head is const term)
    deriving (Eq, Ord, Enum, Bounded, Show)

data Poly a = Poly 
    { endianness :: !Endianness
    , _trimmed   :: !Bool
    , coeffs     :: ![a]
    }

instance Num a => Show (Poly a) where
    showsPrec p (trim (0==) -> Poly end _ cs) 
        = showParen (p > 10) 
            ( showString "poly "
            . showsPrec 11 end
            . showChar ' '
            . showsPrec 11 cs
            )

instance (Num a, Eq a) => Eq (Poly a) where
    p == q  
        | endianness p == endianness q
        = coeffs (trim (0==) p) == coeffs (trim (0==) q)
        | otherwise 
        = polyCoeffs BE p == polyCoeffs BE q
        

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
    fmap f (Poly end _ cs) = Poly end False (map f cs)

instance AdditiveGroup a => AdditiveGroup (Poly a) where
    zeroV = Poly LE True []
    (rawPolyCoeffs LE ->  a) ^+^ (rawPolyCoeffs LE ->  b) 
        = Poly LE False (zipSumV a b)
    negateV = fmap negateV

instance VectorSpace a => VectorSpace (Poly a) where
    type Scalar (Poly a) = Scalar a
    (*^) s = fmap (s *^)
