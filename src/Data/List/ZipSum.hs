module Data.List.ZipSum where

import Data.AdditiveGroup

-- like @zipWith (+)@ except that when the end of either list is
-- reached, the rest of the output is the rest of the longer input list.
zipSum :: Num t => [t] -> [t] -> [t]
zipSum xs [] = xs
zipSum [] ys = ys
zipSum (x:xs) (y:ys) = (x+y) : zipSum xs ys

-- like @zipWith (^+^)@ except that when the end of either list is
-- reached, the rest of the output is the rest of the longer input list.
zipSumV :: AdditiveGroup t => [t] -> [t] -> [t]
zipSumV xs [] = xs
zipSumV [] ys = ys
zipSumV (x:xs) (y:ys) = (x^+^y) : zipSumV xs ys

