{-# LANGUAGE TemplateHaskell, TypeFamilies, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.VectorSpace.WrappedNum
  (WrappedNum(..)) where

import Data.VectorSpace
import qualified Data.Vector.Unboxed         as U
-- Template Haskell in GHC 7.4 requires these imports to bring
-- the `Vector` and `MVector` classes into scope
import qualified Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable

import Data.Vector.Unboxed.Deriving

newtype WrappedNum a = WrapNum { unwrapNum :: a }
    deriving
        (Eq, Ord, Read, Show, Bounded
        , Enum, Num, Fractional, Real, RealFrac
        , Floating, RealFloat)

derivingUnbox "Wrapped"
    [t| (U.Unbox a) => WrappedNum a -> a |] [| unwrapNum |] [| \ a -> WrapNum a |]

instance Num a => AdditiveGroup (WrappedNum a) where
    zeroV = 0
    (^+^) = (+)
    negateV = negate

instance Num a => VectorSpace (WrappedNum a) where
    type Scalar (WrappedNum a) = WrappedNum a
    (*^) = (*)
