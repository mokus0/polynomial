{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, StandaloneDeriving, MultiParamTypeClasses, FlexibleContexts #-}
module Data.VectorSpace.WrappedNum where

import Data.VectorSpace
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as G

newtype WrappedNum a = WrapNum { unwrapNum :: a }
    deriving
        (Eq, Ord, Read, Show, Bounded
        , Enum, Num, Fractional, Real, RealFrac
        , Floating, RealFloat)

deriving instance G.Vector  U.Vector  a => G.Vector  U.Vector  (WrappedNum a)
deriving instance G.MVector U.MVector a => G.MVector U.MVector (WrappedNum a)
deriving instance U.Unbox a => U.Unbox (WrappedNum a)

instance Num a => AdditiveGroup (WrappedNum a) where
    zeroV = 0
    (^+^) = (+)
    negateV = negate

instance Num a => VectorSpace (WrappedNum a) where
    type Scalar (WrappedNum a) = WrappedNum a
    (*^) = (*)
