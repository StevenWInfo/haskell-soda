{-# LANGUAGE OverloadedStrings #-}
module QueryTests
    ( tests
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Function ((&))

import Soda
import Query
import Datatypes

{-
 - Possibly rename this. Possibly network or something.
 -}

{-
tests :: TestTree
tests = testGroup "Soda Tests"
    [ testCase "Big Query test" $
        queryToParam emptyQuery @?= ""
    ]
    -}
