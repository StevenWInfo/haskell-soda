module QueryTests
    ( tests
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import Soda
import Query
import Datatypes
import SodaFunctions

tests :: TestTree
tests = testGroup "Query Tests"
    [ testCase "Smoke test for query" $
        queryToString emptyQuery @?= ""
    , testCase "Basic filter query test" $
        (queryToString $ emptyQuery { filters = Just [ colFoo $= (SodaVal "Bar") ]}) @?= "Foo='Bar'"
    , testCase "Filter query test with a Soda Function" $
        (queryToString $ emptyQuery { filters = Just [ numCol $= avgCol ]}) @?= "Num=avg(Baz)"
    , testCase "Testing select parameter building" $
        (queryToString $ emptyQuery { selects = Just [ Select colFoo, Alias numCol "NumAlias" ]}) @?= "$select=Foo, Num as NumAlias"
    , testCase "Testing group parameter building" $
        (queryToString $ emptyQuery { groups = Just [ Groupify colFoo, Groupify numCol ]}) @?= "$group=Foo, Num"
    , testCase "Testing order parameter building" $
        (queryToString $ emptyQuery { orders = Just [ Order colFoo ASC, Order numCol DESC ]}) @?= "$order=Foo ASC, Num DESC"
    , testCase "Testing limit parameter building" $
        (queryToString $ emptyQuery { limit = Just 4}) @?= "$limit=4"
    , testCase "Testing offset parameter building" $
        (queryToString $ emptyQuery { offset = Just 6}) @?= "$offset=6"
    , testCase "Testing search parameter building" $
        (queryToString $ emptyQuery { search = Just "Lorem Ipsum"}) @?= "$q=Lorem Ipsum"
    , testCase "Testing BOM parameter building" $
        (queryToString $ emptyQuery { bom = Just True}) @?= "$$bom=true"
    , testCase "Testing all query parts" $
        (queryToString 
            $ emptyQuery { filters = Just [ numCol $= (SodaVal (SodaNum 5.0))]
                         , selects = Just [ Select numCol, Alias avgCol "average"]
                         , groups  = Just [ Groupify colFoo ]
                         , orders  = Just [ Order numCol ASC ]
                         , offset  = Just 5
                         , limit   = Just 3
                         , search  = Just "Hello, world"
                         , bom     = Just False
                         }
        ) @?= "Num=5.0&$select=Num, avg(Baz) as average&$order=Num ASC&$group=Foo&$limit=3&$offset=5&$q=Hello, world&$$bom=false"
    -- Might want to move these tests to another file.
    , testCase "Testing getting values out of responses" $
        getVal ((Expr $ SodaVal "Foobar") :: Expr String) @?= Right "Foobar"
    , testCase "Testing getting error for incorrect input to getVal" $
        getVal (Expr colFoo) @?= Left BadLower
    ]
    where colFoo = Column "Foo" :: Column SodaText
          numCol = Column "Num" :: Column SodaNum
          avgCol = Avg ((Column "Baz") :: Column SodaNum)
