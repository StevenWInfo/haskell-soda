{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Soda

import System.IO
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text (Text)
import Network.HTTP.Req
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types.Status as Status
import Control.Exception

import qualified QueryTests

{-
 - Break this into multiple files soon.
 -}

main :: IO ()
main = defaultMain $ testGroup "haskell-soda" 
    [ unitTests
    , QueryTests.tests
    ]

-- The test that actually queries isn't a great test because it will fail if the dataset changes, but it's useful now.
unitTests = testGroup "Unit tests"
    [ testCase "Try out executing query." $ do
        contents <- readFile "tests/data/4tka-6guvMag3.json"
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
    ]
    where
        selectNotFound :: HttpException -> Maybe Status.Status
        selectNotFound (VanillaHttpException (Http.HttpExceptionRequest _ (Http.StatusCodeException (rec) _))) = Just (Http.responseStatus rec)
        selectNotFound _ = Nothing

{-
    -- I'll probably need to use the status code and response header later
    putStrLn $ "The status code was: " ++
               show (Http.getResponseStatusCode response)
    print $ Http.getResponseHeader "Content-Type" response
-}

testDomain :: Domain
testDomain = "soda.demo.socrata.com"

testDataset :: DatasetID
testDataset = "4tka-6guv"

testFormat :: ResponseFormat
testFormat = JSON

testQuery :: RawParameters
testQuery = [("$where", "magnitude > 3.0")]

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
