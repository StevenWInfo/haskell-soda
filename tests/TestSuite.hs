{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import qualified QueryTests
import qualified SodaTests

main :: IO ()
main = defaultMain $ testGroup "haskell-soda" 
    [ QueryTests.tests
    , SodaTests.tests
    ]
