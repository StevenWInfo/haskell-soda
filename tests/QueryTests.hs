{-# LANGUAGE OverloadedStrings #-}
module QueryTests
    ( tests
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import Soda
import Query
import Datatypes
import SodaFunctions

{-
The types that I've set up have corrected several incorrect tests that I've made. It's rather satisfying and reassuring to have the library that is made to assure the correctness of queries, corrects its author on how the queries should be made.
 -}

tests :: TestTree
tests = testGroup "Query Tests"
    [ testCase "Smoke test for query" $
        queryToString emptyQuery @?= ""
    , testCase "Basic filter query test" $
        (queryToString $ emptyQuery { filters = Just [ colFoo $= (SodaVal "Bar") ]}) @?= "Foo='Bar'"
    , testCase "Filter query test with a Soda Function." $
        (queryToString $ emptyQuery { filters = Just [ numCol $= avgCol ]}) @?= "Num=avg(Baz)"
    --, testCase "Testing select parameter building." $
        --(queryToString $ emptyQuery { selects = Just [ Select colFoo, Alias numCol "NumAlias" ]}) @?= "$select=Foo, Num as NumAlias"
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
            $ emptyQuery { filters = Just [ numCol $= (SodaVal (SodaNum 5.0))]
                         --, selects = Just [ Select numCol, Alias avgCol "average"]
                         , groups  = Just [ Groupify colFoo ]
                         , orders  = Just [ Order numCol ASC ]
                         , offset  = Just 5
                         , limit   = Just 3
                         , search  = Just "Hello, world"
                         , bom     = Just False
                         }
        --) @?= "Num=5.0&$select=Num, avg(Baz) as average&$order=Num ASC&$group=Foo&$limit=3&$offset=5&$q=Hello, world&$$bom=false"
        ) @?= "Num=5.0&$order=Num ASC&$group=Foo&$limit=3&$offset=5&$q=Hello, world&$$bom=false"
    -- Might want to move these tests to another file.
    , testCase "Testing getting values out of responses." $
        getVal (some_val  selectA) @?= Right "Foobar"
    , testCase "Testing getting error for incorrect input to getVal." $
        getVal (source  selectA) @?= Left BadLower
    ]
    where colFoo = Column "Foo" :: Column SodaText
          numCol = Column "Num" :: Column SodaNum
          avgCol = Avg ((Column "Baz") :: Column SodaNum)

-- Not sure if either option will even work yet.

-- One option: use Expr
-- Will have to give some way to decode, which is simple.
-- Pros: This will probably be simpler to code
--       The types are baked into the record type
-- Cons: User will have to lift any instance with Expr. Could advise to make a helper function if they do it a lot, but that's not great.
data TestSelectA = TestSelectA { source      :: Expr SodaText
                               , shake_power :: Expr SodaNum
                               , some_val    :: Expr SodaText
                               , some_expr   :: Expr SodaText
                               }

selectA = TestSelectA { source = Expr (Column "source")
                      , shake_power = Expr $ ((Column "magnitude") :: Column SodaNum) $+ SodaVal (SodaNum 4.5)
                      , some_val = Expr $ SodaVal "Foobar"
                      , some_expr = Expr $ SodaVal "Lorem" $++ SodaVal " ipsum"
                      }

-- Another option: Possibly add a lot of type params.
-- I don't think this one will work because of the types and how they would work with the "execute query" function.
-- Pros: The types of the fields will sort of be in the type parameters
-- Cons: The type going into the "Execute Query" function often won't be the same as the type coming out.
--       Have to manually declare type coming out of "execute query" function. Could make a mistake here (although caught before production if tested).
data TestSelectB a b c = TestSelectB { region   :: a
                                     , deepness :: b
                                     , whatever :: c
                                     }

selectB = TestSelectB { region = Column "region" :: Column SodaText
                      , deepness = ((Column "depth") :: Column SodaNum) $+ SodaVal (SodaNum 30)
                      , whatever = SodaVal "abc" $++ SodaVal "xyz"
                      }

-- Option 3: Have them create GADTs with field types that have constructor type restrictions for the SodaExpr typeclass. This seems less convenient.
