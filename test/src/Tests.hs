#!/usr/bin/env runhaskell
module Main where

import qualified BernsteinTests
import qualified PolynomialTests
import Test.Framework (defaultMain, testGroup)

main = defaultMain 
    [ testGroup "Math.Polynomial" PolynomialTests.tests
    , testGroup "Math.Polynomial.Bernstein" BernsteinTests.tests
    ]