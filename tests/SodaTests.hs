{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module SodaTests
    ( tests
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import GHC.Generics
import Data.Aeson ((.:))
import Data.Aeson
import Data.Function ((&))
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
    [ testCase "Try out executing query." $ do
        contents <- readFile "tests/data/6yvf-kk3nLimit3.json"
        response <- getStringBody testDomain testDataset testFormat testQuery
        response @?= (init contents)
    , testCase "Trying out getting header out of response." $ do
        response <- try (getLbsResponse testDomain testDataset testFormat testQuery) :: IO (Either HttpException LbsResponse)
        case response of
            Left ex -> do
                False @? "Shouldn't have thrown exception."
            Right foo -> do
                ((read (BS.unpack (fromJust $ responseHeader foo "X-Soda2-Types"))) :: [String]) @?= ["Something"]
    , testCase "Test 404 response" $ do
        response <- try (getLbsResponse (tail testDomain) testDataset testFormat testQuery) :: IO (Either HttpException LbsResponse)
        case response of
            Left ex -> do 
                          True @? "Threw exception" -- unnecessary
            Right foo -> do
                            False @? "Should have thrown exception"
    , testCase "Test 404 response" $ do
        response <- tryJust selectNotFound (getLbsResponse ((tail . tail) testDomain) testDataset testFormat testQuery) -- :: IO (Either HttpException LbsResponse)
        case response of
            Left code -> do 
                            code @?= Status.status404
                            --putStrLn (code ++ " Foobar")
            Right _ -> do
                            False @? "Should have thrown exception"
    , testCase "Testing call to dataset with query type." $ do
        let query1 = queryToParam $ emptyQuery { limit = Just 1 }
        response <- getStringBody testDomain testDataset testFormat query1
        response @?= result
    -- I'm testing this in SodaTests.hs but it's defined in Query.hs?
    -- I know I can probably do this with fold or something instead.
    , testCase "Testing FromJSON decoding for SodaTypes" $
        case (eitherDecode response :: Either String [TestSelectA]) of
            Right decoded -> do 
                eval <- sequence $ map (\row -> do
                        getVal (source row) @?= getVal (source responseVal)
                        getVal (shake_power row) @?= getVal (shake_power responseVal)
                        getVal (some_val row) @?= getVal (some_val responseVal)
                        getVal (some_expr row) @?= getVal (some_expr responseVal)
                    ) decoded
                return (head eval)
            Left err -> do
                False @? "Decoding error: " ++ err
    , testCase "Testing if the JSON is well formed" $
        case (decode responseTest :: Maybe [TestJson]) of
            Just decoded -> do
                eval <- sequence $ map (\row -> do
                        sourceA row @?= "ak"
                        shake_powerA row @?= 1.6
                        some_valA row @?= "36km W of Valdez, Alaska"
                        some_exprA row @?= "ak11243041"
                    ) decoded
                return (head eval)
            _ -> do
                False @? "JSON malformed"
    , testCase "Testing if the JSON is well formed" $
        case (decode rowTest :: Maybe TestJson) of
            Just decoded -> do
                sourceA decoded @?= "ak"
                shake_powerA decoded @?= 1.6
                some_valA decoded @?= "36km W of Valdez, Alaska"
                some_exprA decoded @?= "ak11243041"
            _ -> do
                False @? "JSON malformed"
    , testCase "Testing full API call" $ do
        theResponse <- getSodaResponse testDomain testDataset $ emptyQuery { filters = Just [ (Column "magnitude" :: Column SodaNum) $= (SodaVal $ SodaNum 1.6) ], limit = Just 1 }
        (show theResponse) @?= "Foobar"
    ]
    where
        testDomain  = "soda.demo.socrata.com"
        testDataset = "6yvf-kk3n"
        testFormat  = JSON
        testQuery   = [("$where", "earthquake_id in('ak11243041', 'ak11246151', 'ak11246918')")]
        result      = "[{\"depth\":\"0\",\"earthquake_id\":\"ak11243041\",\"magnitude\":\"1.6\",\"number_of_stations\":\"6\",\"region\":\"36km W of Valdez, Alaska\",\"source\":\"ak\"}]\n"
        response    = "[{\"some_expr\":\"ak11243041\",\"shake_power\":\"1.6\",\"some_val\":\"36km W of Valdez, Alaska\",\"source\":\"ak\"}]"
        responseTest = "[{\"some_exprA\":\"ak11243041\",\"shake_powerA\":\"1.6\",\"some_valA\":\"36km W of Valdez, Alaska\",\"sourceA\":\"ak\"}]"
        rowTest = "{\"some_exprA\":\"ak11243041\",\"shake_powerA\":\"1.6\",\"some_valA\":\"36km W of Valdez, Alaska\",\"sourceA\":\"ak\"}"
        responseVal = TestSelectA { source = Expr . SodaVal $ "ak"
                                  , shake_power = Expr . SodaVal $ SodaNum 1.6
                                  , some_val = Expr . SodaVal $ "36km W of Valdez, Alaska"
                                  , some_expr = Expr . SodaVal $ "ak11243041"
                                  }
        selectNotFound :: HttpException -> Maybe Status.Status
        selectNotFound (VanillaHttpException (Http.HttpExceptionRequest _ (Http.StatusCodeException (rec) _))) = Just (Http.responseStatus rec)
        selectNotFound _ = Nothing

data TestSelectA = TestSelectA { source      :: Expr SodaText
                               , shake_power :: Expr SodaNum
                               , some_val    :: Expr SodaText
                               , some_expr   :: Expr SodaText
                               } deriving (Generic, Show)

instance FromJSON TestSelectA where
    parseJSON (Object v) = TestSelectA <$>
                                v .: "source" <*>
                                v .: "shake_power" <*>
                                v .: "some_val" <*>
                                v .: "some_expr"
    parseJSON _          = mempty

selectA = TestSelectA { source      = Expr (Column "source")
                      , shake_power = Expr $ ((Column "magnitude") :: Column SodaNum) $+ SodaVal (SodaNum 4.5)
                      , some_val    = Expr $ SodaVal "Foobar"
                      , some_expr   = Expr $ SodaVal "Lorem" $++ SodaVal " ipsum"
                      }

data TestJson = TestJson { sourceA :: String
                         , shake_powerA :: Double
                         , some_valA :: String
                         , some_exprA :: String
                         } deriving (Generic, Show)

instance FromJSON TestJson
