{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module SodaTests
    ( tests
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import GHC.Generics
import Data.Aeson ((.:))
import qualified Data.Aeson as Json
import Data.Function ((&))
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
    , testCase "Test 404 response" $ do
        response <- try (getStringResponse (tail testDomain) testDataset testFormat testQuery) :: IO (Either HttpException LbsResponse)
        case response of
            Left ex -> do 
                          True @? "Threw exception" -- unnecessary
            Right foo -> do
                            False @? "Should have thrown exception"
    , testCase "Test 404 response" $ do
        response <- tryJust selectNotFound (getStringResponse ((tail . tail) testDomain) testDataset testFormat testQuery) -- :: IO (Either HttpException LbsResponse)
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
    ]
    where
        testDomain  = "soda.demo.socrata.com"
        testDataset = "6yvf-kk3n"
        testFormat  = JSON
        testQuery   = [("$where", "earthquake_id in('ak11243041', 'ak11246151', 'ak11246918')")]
        result      = "[{\"depth\":\"0\",\"earthquake_id\":\"ak11243041\",\"magnitude\":\"1.6\",\"number_of_stations\":\"6\",\"region\":\"36km W of Valdez, Alaska\",\"source\":\"ak\"}]\n"
        response    = "[{\"some_expr\":\"ak11243041\",\"shake_power\":\"1.6\",\"some_val\":\"36km W of Valdez, Alaska\",\"source\":\"ak\"}]\n"
        selectNotFound :: HttpException -> Maybe Status.Status
        selectNotFound (VanillaHttpException (Http.HttpExceptionRequest _ (Http.StatusCodeException (rec) _))) = Just (Http.responseStatus rec)
        selectNotFound _ = Nothing

data TestSelectA = TestSelectA { source      :: Expr SodaText
                               , shake_power :: Expr SodaNum
                               , some_val    :: Expr SodaText
                               , some_expr   :: Expr SodaText
                               } deriving (Generic, Show)

instance Json.FromJSON TestSelectA where
    parseJSON (Json.Object v) = TestSelectA <$>
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
{-
data Earthquake =
    Earthquake { source :: String
               , earthquake_id :: String
               , version :: String
               , magnitude :: Double
               , depth :: Double
               , number_of_stations :: Double
               , region :: String
               , location_city :: String
               , location :: String
               , location_address :: String
               , location_zip :: String
               , location_state :: String
               } deriving (Show, Generic)

--instance FromJSON Earthquake
instance FromJSON Earthquake where
    parseJSON (Object v) =
        Earthquake <$> v .:? "source" .!= ""
                   <*> v .:? "earthquake_id" .!= ""
                   <*> v .:? "version" .!= ""
                   <*> v .:? "magnitude" .!= ""
                   <*> v .:? "depth" .!= ""
                   <*> v .:? "number_of_stations" .!= ""
                   <*> v .:? "region" .!= ""
                   <*> v .:? "location_city" .!= ""
                   <*> v .:? "location" .!= ""
                   <*> v .:? "location_address" .!= ""
                   <*> v .:? "location_zip" .!= ""
                   <*> v .:? "location_state" .!= ""
    parseJSON _ = mzero

instance ToJSON Earthquake
-}
