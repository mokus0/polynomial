{-# LANGUAGE ExtendedDefaultRules #-}
module Tests.Core (coreTests) where

import Data.List
import Data.VectorSpace
import Math.Polynomial
import Math.Polynomial.Lagrange
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck
import TestUtils

-- Use exact math everywhere; we're not testing stability or speed,
-- just mathematical soundness.
default (Integer, Rational)

coreTests = 
    [ testGroup "constants"
        [ testGroup "zero"
            [ testCase "polyIsZero zero" (assert (polyIsZero zero))
            , testCase "polyDegree zero == (-1)" (assert (polyDegree zero == (-1)))
            , testProperty "(p == zero) == polyIsZero p" $ \p ->
                (p == zero) == polyIsZero p
            , testProperty "evalPoly zero x == 0" $ \x ->
                evalPoly zero x == 0
            ]
        , testGroup "one"
            [ testCase "polyIsOne one" (assert (polyIsOne one))
            , testCase "polyDegree one == 0" (assert (polyDegree one == 0))
            , testProperty "(p == one) == polyIsOne p" $ \p ->
                (p == one) == polyIsOne p
            , testProperty "evalPoly one x == 1" $ \x ->
                evalPoly one x == 1
            ]
        , testGroup "x"
            [ testProperty "evalPoly x t == t" $ \t ->
                evalPoly x t == t
            , testCase "polyDegree x == 1" (assert (polyDegree x == 1))
            ]
        , testGroup "constPoly"
            [ testProperty "evalPoly (constPoly x) == const x" $ \a b -> 
                evalPoly (constPoly a) b == const a b
            , testProperty "polyDegree (constPoly x) == if x == 0 then -1 else 0" $ \x -> 
                polyDegree (constPoly x) == if x == 0 then -1 else 0
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
        , testProperty "degree" $ \p q -> 
            let n = polyDegree p
                m = polyDegree q
                r = addPoly p q
             in if n /= m
                || polyIsZero p || polyIsZero q
                || head (polyCoeffs BE p) + head (polyCoeffs BE q) /= 0
                    then polyDegree r == max m n
                    else polyDegree r <  max m n
        ]
    , testGroup "sumPoly"
        [ testProperty "sane" $ \ps -> sumPolys ps == foldl' addPoly zero ps
        ]
    , testGroup "negatePoly"
        [ testProperty "sane" $ \p -> polyIsZero (addPoly p (negatePoly p))
        , testProperty "degree" $ \p -> polyDegree p == polyDegree (negatePoly p)
        ]
    , testGroup "composePoly"
        [ testProperty "sane" $ \f g x -> 
            polyDegree f * polyDegree g <= 750 ==>
                    evalPoly (composePoly f g) x 
                 == evalPoly f (evalPoly g x)
        , testProperty "associative" $ \f g h -> 
            polyDegree f * polyDegree g * polyDegree h <= 500 ==>
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
        , testProperty "degree" $ \p q -> 
            polyDegree (composePoly p q) == 
                if polyIsZero p then -1 else
                    if polyIsZero q 
                        then if evalPoly p 0 == 0 then -1 else 0
                        else polyDegree p * polyDegree q
        ]
    , testGroup "scalePoly"
        [ testProperty "sane" $ \s p x ->
            evalPoly (scalePoly s p) x == s * evalPoly p x
        , testProperty "degree" $ \s p -> 
            polyDegree (scalePoly s p) == if s == 0 then -1 else polyDegree p
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
        , testProperty "degree" $ \p q -> 
            if polyIsZero p || polyIsZero q
                then polyDegree (multPoly p q) == (-1)
                else polyDegree (multPoly p q) == polyDegree p + polyDegree q
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
        , testProperty "degree" $ \p (NonNegative n) ->
            let n' = n `mod` 16
             in polyDegree (powPoly p n') == max (-1) (n' * polyDegree p)
        ]
    , testGroup "quotRemPoly"
        [ testProperty "sane" $ \a b -> 
            not (polyIsZero b) ==> case quotRemPoly a b of
                (q, r) -> polyDegree r < polyDegree b 
                       && addPoly (multPoly q b) r == a
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
        [ testProperty "zero"       $ \t   -> evalPolyDeriv zero          t == (0,0)
        , testProperty "one"        $ \t   -> evalPolyDeriv one           t == (1,0)
        , testProperty "constPoly"  $ \t k -> evalPolyDeriv (constPoly k) t == (k,0)
        , testProperty "x"          $ \t   -> evalPolyDeriv x             t == (t,1)
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
                (q, r) -> addPoly (multPoly q (poly BE [1,-a])) (constPoly r) == p
        , testProperty "root" $ \p a ->
            case contractPoly p a of
                (q, r) -> evalPoly (addPoly p (poly BE [-r])) a == 0
        ]
    , testGroup "monicPoly"
        [ testProperty "sane" $ \p ->
            if polyIsZero p 
                then polyIsZero (monicPoly p)
                else head (polyCoeffs BE (monicPoly p)) == 1
        ]
    , testGroup "gcdPoly"
        [ testProperty "sane" $ \p q ->
            (polyDegree p + polyDegree q <= 20) &&
            not (all polyIsZero [p,q]) ==>
                let g = gcdPoly p q
                 in all polyIsZero [p `remPoly` g, q `remPoly` g]
        , testProperty "monic" $ \p q ->
            (polyDegree p + polyDegree q <= 20) &&
            not (all polyIsZero [p,q]) ==>
                head (polyCoeffs BE (gcdPoly p q)) == 1
        , testProperty "right cancel" $ \p -> gcdPoly p one == one
        , testProperty "left  cancel" $ \p -> gcdPoly one p == one
        , testProperty "commutative" $ \p q -> 
            (polyDegree p + polyDegree q <= 20) &&
            not (all polyIsZero [p,q]) ==>
                gcdPoly p q == gcdPoly q p
        , testProperty "associative" $ \p q r -> 
            (polyDegree p + polyDegree q + polyDegree r <= 20) &&
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
        , testProperty "constPoly" $ \k -> 
            polyDeriv (constPoly k) == zero
        , testCase "x" $ do
            assert (polyDeriv x == one)
        , testProperty "chain rule" $ \p q ->
            (polyDegree p + polyDegree q <= 20) ==>
            polyDeriv (multPoly p q) == addPoly (multPoly p (polyDeriv q)) (multPoly q (polyDeriv p))
        ]
    , testGroup "polyIntegral"
        [ testProperty "sane" $ \p -> polyDeriv (polyIntegral p) == p
        , testProperty "constant factor" $ \p -> evalPoly (polyIntegral p) 0 == 0
        ]
    , testGroup "separateRoots"
        [ testProperty "sane" $ \(NonEmpty rts) ->
            length rts < 10 ==>
            separateRoots (lagrange rts) == map lagrange (sep rts)
        ]
    ]
