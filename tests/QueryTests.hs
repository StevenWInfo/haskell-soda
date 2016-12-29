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
        queryToString emptyQuery @?= ""
    , testCase "Basic filter query test" $
        (queryToString $ emptyQuery & replaceFilters [ colFoo === (SodaVal "Bar") ]) @?= "Foo='Bar'"
    , testCase "Filter query test with a Soda Function." $
        (queryToString $ emptyQuery & replaceFilters [ numCol === avgCol ]) @?= "Num=avg(Baz)"
    , testCase "Testing replacing functions and ampersand style." $
        (queryToString $ emptyQuery
            & replaceFilters [ numCol === (SodaVal (Number 5.0)) ]
            & replaceLimit 10
            & replaceOffset 5
            & replaceSearch "Hello, world"
            & replaceBom True
        ) @?= "Num=5.0&$limit=10&$offset=5&$q=Hello, world&$$bom=true"
    , testCase "Testing select parameter building." $
        (queryToString $ emptyQuery & replaceSelects [ Select colFoo, Alias numCol "NumAlias" ]) @?= "$select=Foo, Num as NumAlias"
    , testCase "Testing group parameter building." $
        (queryToString $ emptyQuery & replaceGroups [ Groupify colFoo, Groupify numCol ]) @?= "$group=Foo, Num"
    , testCase "Testing order parameter building." $
        (queryToString $ emptyQuery & replaceOrders [ Order colFoo ASC, Order numCol DESC]) @?= "$order=Foo ASC, Num DESC"
    , testCase "Testing all query parts." $
        (queryToString $ emptyQuery
            & replaceFilters [ numCol === (SodaVal (Number 5.0)) ]
            & replaceLimit 10
            & replaceSelects [ Select numCol, Alias avgCol "average"]
            & replaceGroups [ Groupify colFoo ]
            & replaceOrders [ Order numCol ASC ]
            & replaceOffset 5
            & replaceSearch "Hello, world"
            & replaceBom True
        ) @?= "Num=5.0&$select=Num, avg(Baz) as average&$order=Num ASC&$group=Foo&$limit=10&$offset=5&$q=Hello, world&$$bom=true"
    ]
    where colFoo = Column "Foo" :: Column SodaText
          numCol = Column "Num" :: Column Number
          avgCol = Avg ((Column "Baz") :: Column Number)
