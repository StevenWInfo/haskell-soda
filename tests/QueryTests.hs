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
        (queryToString $ emptyQuery { filters = Just [ colFoo === (SodaVal "Bar") ]}) @?= "Foo='Bar'"
    , testCase "Filter query test with a Soda Function." $
        (queryToString $ emptyQuery { filters = Just [ numCol === avgCol ]}) @?= "Num=avg(Baz)"
    , testCase "Testing select parameter building." $
        (queryToString $ emptyQuery { selects = Just [ Select colFoo, Alias numCol "NumAlias" ]}) @?= "$select=Foo, Num as NumAlias"
    , testCase "Testing group parameter building." $
        (queryToString $ emptyQuery { groups = Just [ Groupify colFoo, Groupify numCol ]}) @?= "$group=Foo, Num"
    , testCase "Testing order parameter building." $
        (queryToString $ emptyQuery { orders = Just [ Order colFoo ASC, Order numCol DESC ]}) @?= "$order=Foo ASC, Num DESC"
    , testCase "Testing limit parameter building." $
        (queryToString $ emptyQuery { limit = Just 4}) @?= "$limit=4"
    , testCase "Testing offset parameter building." $
        (queryToString $ emptyQuery { offset = Just 6}) @?= "$offset=6"
    , testCase "Testing search parameter building." $
        (queryToString $ emptyQuery { search = Just "Lorem Ipsum"}) @?= "$q=Lorem Ipsum"
    , testCase "Testing BOM parameter building." $
        (queryToString $ emptyQuery { bom = Just True}) @?= "$$bom=true"
    , testCase "Testing all query parts." $
        (queryToString 
            $ emptyQuery { filters = Just [ numCol === (SodaVal (Number 5.0))]
                         , selects = Just [ Select numCol, Alias avgCol "average"]
                         , groups  = Just [ Groupify colFoo ]
                         , orders  = Just [ Order numCol ASC ]
                         , offset  = Just 5
                         , limit   = Just 3
                         , search  = Just "Hello, world"
                         , bom     = Just False
                         }
        ) @?= "Num=5.0&$select=Num, avg(Baz) as average&$order=Num ASC&$group=Foo&$limit=3&$offset=5&$q=Hello, world&$$bom=false"
    ]
    where colFoo = Column "Foo" :: Column SodaText
          numCol = Column "Num" :: Column Number
          avgCol = Avg ((Column "Baz") :: Column Number)
