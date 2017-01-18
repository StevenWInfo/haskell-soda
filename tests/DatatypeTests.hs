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
tests = testGroup "Datatype Tests"
    [ testCase "Basic column test" $
        toUrlParam colFoo @?= "Foobar"
    , testCase "Testing lower" $
        lower (SodaVal (Just True)) @?= Right (Just True)
    , testCase "Testing what money looks like" $
        (toUrlParam . SodaVal . Money $ 19.99) @?= "'19.99'"
    , testCase "Testing basic SodaText toUrlParam" $
        (toUrlParam . SodaVal $ "Lorem ipsum") @?= "'Lorem ipsum'"
    , testCase "A SodaText with an apostrophe/single quote in it" $
        (toUrlParam . SodaVal $ "Eat at Joe's") @?= "'Eat at Joe''s'"
    , testCase "Testing what the time looks like in the URL" $
        (toUrlParam . SodaVal . posixSecondsToUTCTime $ fromIntegral 1483426032) @?= "2017-01-03T06:47:12.000"
    , testCase "Testing what point looks like in a URL" $
        (toUrlParam . SodaVal $ Point { longitude = -87.653274, latitude = 41.936172}) @?= "'POINT (-87.653274 41.936172)'"
    , testCase "Testing what line looks like in a URL" $
        (toUrlParam . SodaVal $ Line [point (-87.653274) 41.936172, point (-80.5) 40.4]) @?= "'LINESTRING (-87.653274 41.936172, -80.5 40.4)'"
    , testCase "Testing what multiline looks like in a URL" $
        (toUrlParam . SodaVal $ [Line [point (-87.653274) 41.936172, point (-80.5) 40.4], Line [point 2 3, point 4 5]]) @?= "'MULTILINESTRING ((-87.653274 41.936172, -80.5 40.4), (2.0 3.0, 4.0 5.0))'"
    , testCase "Testing what Polygon looks like in a URL" $
        (toUrlParam . SodaVal $ Polygon [[point (-67.653274) 48.936172, point (-21.5) 94.4, point 40.1 53.9]]) @?= "'POLYGON ((-67.653274 48.936172, -21.5 94.4, 40.1 53.9))'"
    , testCase "Polygon with multiple parts." $
        (toUrlParam . SodaVal $ Polygon [[point 35.0 10.3, point 45.4 45.4, point 40.1 53.9], [point 4.0 5.3, point 56.7 2.1, point 80.1 800.0]]) @?= "'POLYGON ((35.0 10.3, 45.4 45.4, 40.1 53.9), (4.0 5.3, 56.7 2.1, 80.1 800.0))'"
    , testCase "Polygon with multiple parts." $
        (toUrlParam . SodaVal $ [Polygon [[point 35.0 10.3, point 45.4 45.4, point 40.1 53.9], [point 4.0 5.3, point 56.7 2.1, point 80.1 800.0]], Polygon [[point 3 5, point 7 3, point 3 9, point 0 1], [point 4 9, point 5 5], [point 2 1, point 5 9, point 9 3]]]) @?= "'MULTIPOLYGON (((35.0 10.3, 45.4 45.4, 40.1 53.9), (4.0 5.3, 56.7 2.1, 80.1 800.0)), ((3.0 5.0, 7.0 3.0, 3.0 9.0, 0.0 1.0), (4.0 9.0, 5.0 5.0), (2.0 1.0, 5.0 9.0, 9.0 3.0)))'"
    ]
    where colFoo = (Column "Foobar") :: Column SodaText
          point long lat = Point { longitude = long, latitude = lat }
