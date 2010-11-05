#!/usr/bin/env runhaskell
module Main where

import Tests.Bernstein
import Tests.Core
import Test.Framework (defaultMain, testGroup)

main = defaultMain 
    [ testGroup "Math.Polynomial"           coreTests
    , testGroup "Math.Polynomial.Bernstein" bernsteinTests
    ]