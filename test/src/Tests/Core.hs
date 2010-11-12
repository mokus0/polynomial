{-# LANGUAGE ExtendedDefaultRules, TypeSynonymInstances, TypeFamilies #-}
module Tests.Core (coreTests) where

import Control.Applicative
import Data.List
import Data.VectorSpace
import Math.Polynomial
import Math.Polynomial.Lagrange
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

-- Use exact math everywhere; we're not testing stability or speed,
-- just mathematical soundness.
default (Integer, Rational)

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

order p = length (polyCoeffs LE p)

rev BE = LE
rev LE = BE

sep [] = []
sep rts = nub rts : sep (rts \\ nub rts)

coreTests = 
    [ testGroup "constants"
        [ testGroup "zero"
            [ testCase "polyIsZero zero" (assert (polyIsZero zero))
            , testProperty "(p == zero) == polyIsZero p" $ \p ->
                (p == zero) == polyIsZero p
            , testProperty "evalPoly zero x == 0" $ \x ->
                evalPoly zero x == 0
            ]
        , testGroup "one"
            [ testCase "polyIsOne one" (assert (polyIsOne one))
            , testProperty "(p == one) == polyIsOne p" $ \p ->
                (p == one) == polyIsOne p
            , testProperty "evalPoly one x == 1" $ \x ->
                evalPoly one x == 1
            ]
        , testGroup "x"
            [ testProperty "evalPoly x t == t" $ \t ->
                evalPoly x t == t
            ]
        , testGroup "constPoly"
            [ testProperty "evalPoly (constPoly x) == const x" $ \a b -> 
                evalPoly (constPoly a) b == const a b
            ]
        ]
    , testGroup "constructors"
        [ testProperty "polyCoeffs LE . poly LE" $ \cs ->
            case stripPrefix (polyCoeffs LE (poly LE cs)) cs of
                Just zs -> all (== 0) zs
                Nothing -> False
        , testProperty "polyCoeffs LE . poly BE" $ \cs ->
            case stripPrefix (polyCoeffs LE (poly BE cs)) (reverse cs) of
                Just zs -> all (== 0) zs
                Nothing -> False
        , testProperty "polyCoeffs BE . poly LE" $ \cs ->
            case stripPrefix (reverse (polyCoeffs BE (poly LE cs))) cs of
                Just zs -> all (== 0) zs
                Nothing -> False
        , testProperty "polyCoeffs BE . poly BE" $ \cs ->
            case stripPrefix (reverse (polyCoeffs BE (poly BE cs))) (reverse cs) of
                Just zs -> all (== 0) zs
                Nothing -> False
        ]
    , testGroup "instances"
        [ testGroup "Eq"
            [ testProperty "reflexive"  $ \p     -> p == p
            , testProperty "symmetric"  $ \p q   -> (p==q) == (q==p)
            , testProperty "transitive" $ \p q r -> (p == q && q == r) ==> p == r
            , testProperty "sane"       $ \cs ds end1 end2 -> 
                let p = poly end1 cs; q = poly end1 ds
                 in (p==q) == (polyCoeffs end2 p == polyCoeffs end2 q)
            , testProperty "endianness-independent" $ \cs end ->
                poly end cs == poly (rev end) (reverse cs)
            ]
        , testGroup "AdditiveGroup"
            [ testGroup "zeroV"
                [ testCase "polyIsZero zeroV" (assert (polyIsZero zeroV))
                , testProperty "(p == zeroV) == polyIsZero p" $ \p ->
                    (p == zeroV) == polyIsZero p
                , testProperty "evalPoly zeroV x == zeroV" $ \x ->
                    evalPoly zeroV x == zeroV
                ]
            , testGroup "^+^"
                [ testProperty "left  unit"  $ \p     -> zeroV ^+^ p == p
                , testProperty "right unit"  $ \p     -> p ^+^ zeroV == p
                , testProperty "commutative" $ \p q   -> p ^+^ q == q ^+^ p
                , testProperty "associative" $ \p q r ->
                    p ^+^ (q ^+^ r) == (p ^+^ q) ^+^ r
                , testProperty "sane" $ \p q x ->
                    evalPoly (p ^+^ q) x ==
                    evalPoly p x ^+^ evalPoly q x
                ]
            , testGroup "negateV"
                [ testProperty "sane" $ \p -> p ^+^ negateV p == zeroV
                ]
            ]
        , testGroup "VectorSpace"
            [ testProperty "sane" $ \s p x ->
                evalPoly (s *^ (p :: Poly Rational)) x == s *^ evalPoly p x
            ]
        ]
    , testGroup "addPoly"
        [ testProperty "left  unit"  $ \p     -> addPoly zero p == p
        , testProperty "right unit"  $ \p     -> addPoly p zero == p
        , testProperty "commutative" $ \p q   -> addPoly p q == addPoly q p
        , testProperty "associative" $ \p q r ->
            addPoly p (addPoly q r) == addPoly (addPoly p q) r
        , testProperty "sane" $ \p q x ->
            evalPoly (addPoly p q) x ==
            evalPoly p x + evalPoly q x
        ]
    , testGroup "sumPoly"
        [ testProperty "sane" $ \ps -> sumPolys ps == foldl' addPoly zero ps
        ]
    , testGroup "negatePoly"
        [ testProperty "sane" $ \p -> polyIsZero (addPoly p (negatePoly p))
        ]
    , testGroup "composePoly"
        [ testProperty "sane" $ \f g x -> 
            order f * order g <= 750 ==>
                    evalPoly (composePoly f g) x 
                 == evalPoly f (evalPoly g x)
        , testProperty "associative" $ \f g h -> 
            order f * order g * order h <= 1000 ==>
                    composePoly f (composePoly g h)
                 == composePoly (composePoly f g) h
        , testProperty "left  cancel" $ \p k ->
            composePoly p (constPoly k) == constPoly (evalPoly p k)
        , testProperty "right cancel" $ \k p ->
            composePoly (constPoly k) p == constPoly k
        , testProperty "left  identity" $ \p ->
            composePoly p x == p
        , testProperty "right identity" $ \p ->
            composePoly x p == p
        ]
    , testGroup "scalePoly"
        [ testProperty "sane" $ \s p x ->
            evalPoly (scalePoly s p) x == s * evalPoly p x
        ]
    , testGroup "multPoly"
        [ testProperty "left  cancel" $ \p     -> polyIsZero (multPoly zero p)
        , testProperty "right cancel" $ \p     -> polyIsZero (multPoly p zero)
        , testProperty "left  unit"   $ \p     -> multPoly one p == p
        , testProperty "right unit"   $ \p     -> multPoly p one == p
        , testProperty "commutative"  $ \p q   -> multPoly p q == multPoly q p
        , testProperty "associative"  $ \p q r ->
            multPoly p (multPoly q r) == multPoly (multPoly p q) r
        , testProperty "distributive" $ \p q r ->
            multPoly p (addPoly q r) == addPoly (multPoly p q) (multPoly p r)
        , testProperty "sane" $ \p q x ->
            evalPoly (multPoly p q) x ==
            evalPoly p x * evalPoly q x
        ]
    , testGroup "powPoly"
        [ testProperty "cancel"   $ \p -> polyIsOne (powPoly p 0)
        , testProperty "unit"     $ \p -> powPoly p 1 == p
        , testProperty "multiply" $ \p (NonNegative a) (NonNegative b) ->
            let a' = a `mod` 8; b' = b `mod` 8
             in multPoly (powPoly p a') (powPoly p b') == powPoly p (a' + b')
        , testProperty "compose"  $ \p (NonNegative a) (NonNegative b) ->
            let a' = a `mod` 6; b' = b `mod` 6
             in powPoly (powPoly p b') a' == powPoly p (a' * b')
        , testProperty "sane"     $ \p (NonNegative n) ->
            let n' = n `mod` 16
             in powPoly p n' == foldl' multPoly one (replicate n' p)
        ]
    , testGroup "quotRemPoly"
        [ testProperty "sane" $ \a b -> 
            not (polyIsZero b) ==> case quotRemPoly a b of
                (q, r) -> addPoly (multPoly q b) r == a
        ]
    , testGroup "quotPoly"
        [ testProperty "sane" $ \a b -> 
            not (polyIsZero b) ==> 
            quotPoly a b == fst (quotRemPoly a b)
        ]
    , testGroup "remPoly"
        [ testProperty "sane" $ \a b -> 
            not (polyIsZero b) ==> 
            remPoly a b == snd (quotRemPoly a b)
        ]
    , testGroup "evalPolyDeriv"
        [ testProperty "zero" $ \t -> evalPolyDeriv zero t == (0,0)
        , testProperty "one"  $ \t -> evalPolyDeriv one  t == (1,0)
        , testProperty "x"    $ \t -> evalPolyDeriv x    t == (t,1)
        , testProperty "chain rule" $ \p q x ->
            snd (evalPolyDeriv (multPoly p q) x)
            == snd (evalPolyDeriv p x) * evalPoly q x + snd (evalPolyDeriv q x) * evalPoly p x
        , testProperty "sane" $ \p x ->
            evalPolyDeriv p x == (evalPoly p x, evalPoly (polyDeriv p) x)
        ]
    , testGroup "evalPolyDerivs"
        [ testProperty "sane" $ \p x ->
            and $ zipWith (==) 
                (evalPolyDerivs p x) 
                [evalPoly p x | p <- iterate polyDeriv p]
        ]
    , testGroup "contractPoly"
        [ testProperty "sane" $ \p a -> 
            case contractPoly p a of
                (q, r) -> addPoly (multPoly q (poly BE [1,-a])) (poly BE [r]) == p
        , testProperty "root" $ \p a ->
            case contractPoly p a of
                (q, r) -> evalPoly (addPoly p (poly BE [-r])) a == 0
        ]
    , testGroup "gcdPoly"
        [ testProperty "sane" $ \p q ->
            (order p + order q <= 20) &&
            not (all polyIsZero [p,q]) ==>
                let g = gcdPoly p q
                 in all polyIsZero [p `remPoly` g, q `remPoly` g]
        , testProperty "monic" $ \p q ->
            (order p + order q <= 20) &&
            not (all polyIsZero [p,q]) ==>
                head (polyCoeffs BE (gcdPoly p q)) == 1
        , testProperty "right cancel" $ \p -> gcdPoly p one == one
        , testProperty "left  cancel" $ \p -> gcdPoly one p == one
        , testProperty "commutative" $ \p q -> 
            (order p + order q <= 20) &&
            not (all polyIsZero [p,q]) ==>
                gcdPoly p q == gcdPoly q p
        , testProperty "associative" $ \p q r -> 
            (order p + order q + order r <= 20) &&
            not (any (all polyIsZero) [[p,q], [p,r], [q,r]]) ==>
                gcdPoly (gcdPoly p q) r == gcdPoly p (gcdPoly q r)
        , testProperty "roots" $ \pScale (Ordered pRoots) qScale (Ordered qRoots) ->
            (length pRoots + length qRoots <= 20) ==>
            let p = scalePoly pScale (lagrange pRoots)
                q = scalePoly qScale (lagrange qRoots)
                r = lagrange (intersect pRoots qRoots)
             in not (null pRoots && null qRoots)
                ==> r == gcdPoly p q

        ]
    , testGroup "polyDeriv"
        [ testCase "zero" $ do
            assert (polyDeriv zero == zero)
        , testCase "one" $ do
            assert (polyDeriv one == zero)
        , testCase "x" $ do
            assert (polyDeriv x == one)
        , testProperty "chain rule" $ \p q ->
            (order p + order q <= 20) ==>
            polyDeriv (multPoly p q) == addPoly (multPoly p (polyDeriv q)) (multPoly q (polyDeriv p))
        ]
    , testGroup "polyIntegral"
        [ testProperty "sane" $ \p -> polyDeriv (polyIntegral p) == p
        ]
    , testGroup "separateRoots"
        [ testProperty "sane" $ \(NonEmpty rts) ->
            length rts < 10 ==>
            separateRoots (lagrange rts) == map lagrange (sep rts)
        ]
    ]
