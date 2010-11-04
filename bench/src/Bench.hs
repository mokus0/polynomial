#!/usr/bin/env runhaskell
module Main where

import Criterion.Main

import Bench.Core
import Bench.Bernstein
import Bench.Chebyshev
import Bench.Interpolation
import Bench.Lagrange
import Bench.Legendre
import Bench.Newton
import Bench.NumInstance
import Bench.Pretty

main = defaultMain
    [ bgroup "Math"
        [ bgroup "Polynomial" $
            coreTests ++
            [ bgroup "Bernstein"        bernsteinTests
            , bgroup "Chebyshev"        chebyshevTests
            , bgroup "Interpolation"    interpolationTests
            , bgroup "Lagrange"         lagrangeTests
            , bgroup "Legendre"         legendreTests
            , bgroup "Newton"           newtonTests
            , bgroup "NumInstance"      numInstanceTests
            , bgroup "Pretty"           prettyTests
            ]
        ]
    ]