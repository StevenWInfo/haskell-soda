module DatatypeTests
    ( tests
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Soda
import Query
import Datatypes
import SodaFunctions

tests :: TestTree
tests = testGroup "Query Tests"
    [ testCase "Basic column test" $
        toUrlParam colFoo @?= "Foobar"
    , testCase "Testing what money looks like" $
        (toUrlParam . SodaVal . Money $ 19.99) @?= "19.99"
    , testCase "Testing basic SodaText toUrlParam" $
        (toUrlParam . SodaVal $ "Lorem ipsum") @?= "'Lorem ipsum'"
    , testCase "A SodaText with an apostrophe/single quote in it" $
        (toUrlParam . SodaVal $ "Eat at Joe's") @?= "'Eat at Joe''s'"
    , testCase "Testing what the time looks like in the URL" $
        (toUrlParam . SodaVal . posixSecondsToUTCTime $ fromIntegral 1483426032) @?= "2017-01-03T06:47:12.000"
    , testCase "Testing what point looks like in a URL" $
        (toUrlParam . SodaVal $ Point { longitude = -87.653274, latitude = 41.936172}) @?= "'POINT (-87.653274 41.936172)'"
    , testCase "Testing non-case of Location which shouldn't be represented" $
        (toUrlParam . SodaVal $ Location (Just $ point (-80.1) 41.9) (Just $ USAddress {address = "Foo", city = "Anywhere", state = "Upper Montana", zipCode = "55555"})) @?= ""
    , testCase "Testing what line looks like in a URL" $
        (toUrlParam . SodaVal $ Line [point (-87.653274) 41.936172, point (-80.5) 40.4]) @?= "'LINESTRING (-87.653274 41.936172, -80.5 40.4)'"
    , testCase "Testing what multiline looks like in a URL" $
        (toUrlParam . SodaVal $ [Line [point (-87.653274) 41.936172, point (-80.5) 40.4], Line [point 2 3, point 4 5]]) @?= "'MULTILINESTRING ((-87.653274 41.936172, -80.5 40.4), (2.0 3.0, 4.0 5.0))'"
    ]
    where colFoo = (Column "Foobar") :: Column SodaText
          point long lat = Point { longitude = long, latitude = lat }
