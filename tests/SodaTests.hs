{-# LANGUAGE OverloadedStrings #-}

module SodaTests
    ( tests
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Aeson
import Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as BS
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types.Status as Status
import Network.HTTP.Req
import Control.Exception

import Soda
import Query
import Datatypes
import SodaFunctions

{-
 - Possibly rename this. Possibly NetworkTests or something.
 -}

tests :: TestTree
tests = testGroup "Soda Tests"
    [ testCase "Try out executing query" $ do
        contents <- readFile "tests/data/6yvf-kk3nLimit3.json"
        response <- getStringBody testDomain testDataset testFormat testQuery
        response @?= (init contents)
    , testCase "Trying out getting header out of response" $ do
        response <- try (getLbsResponse testDomain testDataset testFormat testQuery) :: IO (Either HttpException LbsResponse)
        case response of
            Left ex -> do
                False @? "Shouldn't have thrown exception"
            Right foo -> do
                ((read (BS.unpack (fromJust $ responseHeader foo "X-Soda2-Types"))) :: [String]) @?= ["number","text","point","text","text","text","text","number","number","text","text","text"]
    , testCase "Test 404 response" $ do
        response <- try (getLbsResponse (tail testDomain) testDataset testFormat testQuery) :: IO (Either HttpException LbsResponse)
        case response of
            Left ex -> do 
                          True @? "Threw exception" -- unnecessary
            Right foo -> do
                            False @? "Should have thrown exception"
    , testCase "Test 404 response" $ do
        response <- tryJust selectNotFound (getLbsResponse ((tail . tail) testDomain) testDataset testFormat testQuery)
        case response of
            Left code -> do 
                            code @?= Status.status404
            Right _ -> do
                            False @? "Should have thrown exception"
    , testCase "Testing call to dataset with query type" $ do
        let query1 = queryToParam $ emptyQuery { limit = Just 1 }
        response <- getStringBody testDomain testDataset testFormat query1
        response @?= result
    , testCase "Testing full API call" $ do
        theResponse <- getSodaResponse testDomain testDataset $ emptyQuery { filters = Just [ (Column "magnitude" :: Column SodaNum) $= (SodaVal $ SodaNum 1.6) ], limit = Just 1 }
        (show theResponse) @?= "[[(\"source\",RSodaText \"ak\"),(\"region\",RSodaText \"36km W of Valdez, Alaska\"),(\"number_of_stations\",RSodaNum (SodaNum {getSodaNum = 6.0})),(\"magnitude\",RSodaNum (SodaNum {getSodaNum = 1.6})),(\"earthquake_id\",RSodaText \"ak11243041\"),(\"depth\",RSodaNum (SodaNum {getSodaNum = 0.0}))]]"
    -- Any value for this is fine as long as the response code is 200.
    , testCase "Testing special characters. (Any value is fine)" $ do
        theResponse <- getSodaResponse testDomain testDataset $ emptyQuery { filters = Just [ (Column "region" :: Column SodaText) $= (SodaVal "a!@#$%^&*(),.;:\"'?+=-_[]{}~`<>\\| ") ], limit = Just 1 }
        (show theResponse) @?= "[]"
    ]
    where
        testDomain  = "soda.demo.socrata.com"
        testDataset = "6yvf-kk3n"
        testFormat  = JSON
        testQuery   = [("$where", "earthquake_id in('ak11243041', 'ak11246151', 'ak11246918')")]
        result      = "[{\"depth\":\"0\",\"earthquake_id\":\"ak11243041\",\"magnitude\":\"1.6\",\"number_of_stations\":\"6\",\"region\":\"36km W of Valdez, Alaska\",\"source\":\"ak\"}]\n"
        selectNotFound :: HttpException -> Maybe Status.Status
        selectNotFound (VanillaHttpException (Http.HttpExceptionRequest _ (Http.StatusCodeException (rec) _))) = Just (Http.responseStatus rec)
        selectNotFound _ = Nothing
