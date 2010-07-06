{-# LANGUAGE ViewPatterns, TypeFamilies #-}
module Math.Polynomial.Type 
    ( Endianness(..)
    , Poly, poly, polyCoeffs
    , polyIsZero, polyIsOne
    ) where

-- import Data.List.Extras.LazyLength
import Data.AdditiveGroup
import Data.VectorSpace
import Data.Cross
import Data.Basis
import Data.List
import Data.List.ZipSum
import Data.Word

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

trim :: Num a => Poly a -> Poly a
trim p@(Poly _ True _) = p
trim   (Poly LE _ cs) = Poly LE True (dropEnd   (==0) cs)
trim   (Poly BE _ cs) = Poly BE True (dropWhile (==0) cs)

-- |Make a 'Poly' from a list of coefficients using the specified coefficient order.
poly :: Num a => Endianness -> [a] -> Poly a
poly end cs = trim (Poly end False cs)

-- |Get the coefficients of a a 'Poly' in the specified order.
polyCoeffs :: Num a => Endianness -> Poly a -> [a]
polyCoeffs end p = case trim p of
    Poly e _ cs | e == end  -> cs
                | otherwise -> reverse cs

polyIsZero :: Num a => Poly a -> Bool
polyIsZero = null . coeffs . trim

polyIsOne :: Num a => Poly a -> Bool
polyIsOne = ([1]==) . coeffs . trim

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
    showsPrec p (trim -> Poly end _ cs) 
        = showParen (p > 10) 
            ( showString "poly "
            . showsPrec 11 end
            . showChar ' '
            . showsPrec 11 cs
            )

instance (Num a, Eq a) => Eq (Poly a) where
    p == q  
        | endianness p == endianness q
        = coeffs (trim p) == coeffs (trim q)
        | otherwise 
        = polyCoeffs BE p == polyCoeffs BE p
        

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

instance Num a => AdditiveGroup (Poly a) where
    zeroV = poly LE []
    (polyCoeffs LE ->  a) ^+^ (polyCoeffs LE ->  b) = poly LE (zipSum a b)
    negateV = fmap negate

instance Num a => VectorSpace (Poly a) where
    type Scalar (Poly a) = a
    (*^) s = fmap (*s)

instance Num a => HasBasis (Poly a) where
    type Basis (Poly a) = Word
    basisValue n = poly BE (1 : genericReplicate n 0)
    decompose = zip [0..] . polyCoeffs LE
    decompose' v n = case genericDrop n (polyCoeffs LE v) of
        []      -> 0
        (x:_)   -> x
