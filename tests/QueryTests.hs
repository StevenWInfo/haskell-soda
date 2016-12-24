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
The types that I've set up have corrected several incorrect tests that I've made. It's rather satisfying and reassuring to have the library that is made to assure the correctness of queries, corrects its author on how the queries should be made.
 -}

tests :: TestTree
tests = testGroup "Query Tests"
    [ testCase "Smoke test for query" $
        queryToParam emptyQuery @?= ""
    , testCase "Basic filter query test" $
        (queryToParam $ emptyQuery & replaceFilters [ colFoo === (SodaVal "Bar") ]) @?= "Foo='Bar'"
    , testCase "Filter query test with a Soda Function." $
        (queryToParam $ emptyQuery & replaceFilters [ numCol === avgCol ]) @?= "Num=avg(Baz)"
    ]
    where colFoo = Column "Foo" :: Column SodaText
          numCol = Column "Num" :: Column Number
          avgCol = Avg ((Column "Baz") :: Column Number)
