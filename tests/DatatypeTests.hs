module DatatypeTests
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
    [ testCase "Basic column test" $
        toUrlParam colFoo @?= "Foobar"
    , testCase "Testing what money looks like." $
        (toUrlParam . SodaVal . Money $ 19.99) @?= "19.99"
    , testCase "Testing basic SodaText toUrlParam." $
        (toUrlParam . SodaVal $ "Lorem ipsum") @?= "'Lorem ipsum'"
    , testCase "A SodaText with an apostrophe/single quote in it." $
        (toUrlParam . SodaVal $ "Eat at Joe's") @?= "'Eat at Joe''s'"
    ]
    where colFoo = (Column "Foobar") :: Column SodaText
