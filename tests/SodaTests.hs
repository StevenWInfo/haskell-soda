
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
 -
 - Also, a lot of these tests are dependent on external datasets and other things, so if those things change, then some tests might fail.
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
                ((read (BS.unpack (fromJust $ responseHeader foo (BS.pack "X-Soda2-Types")))) :: [String]) @?= ["number","text","point","text","text","text","text","number","number","text","text","text"]
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
    -- I used show and strings because I think that I was having trouble making the types an instance of Eq.
    , testCase "Testing full API call" $ do
        theResponse <- getSodaResponse testDomain testDataset $ emptyQuery { filters = Just [ (Column "magnitude" :: Column SodaNum) $= (SodaVal $ SodaNum 1.6) ], limit = Just 1 }
        theResponse @?= [[("source",RSodaText "ak"),("region",RSodaText "36km W of Valdez, Alaska"),("number_of_stations",RSodaNum (SodaNum {getSodaNum = 6.0})),("magnitude",RSodaNum (SodaNum {getSodaNum = 1.6})),("earthquake_id",RSodaText "ak11243041"),("depth",RSodaNum (SodaNum {getSodaNum = 0.0}))]]
    -- Any value for this is fine as long as the response code is 200.
    , testCase "Testing special characters. (Any value is fine)" $ do
        theResponse <- getSodaResponse testDomain testDataset $ emptyQuery { filters = Just [ (Column "region" :: Column SodaText) $= (SodaVal "a!@#$%^&*(),.;:\"'?+=-_[]{}~`<>\\| ") ], limit = Just 1 }
        theResponse @?= []
    , testCase "Testing a handful of SODA functions, operators, and values." $ do
        theResponse <- getSodaResponse testDomain testDataset $
            emptyQuery { selects = Just [ Select source, Alias location "place", Alias (Lower region) "area" ]
                       , limit = Just 3
                       , wheres = Just . Where $ number_of_stations $> sn 1.0 $&& IsNotNull number_of_stations $&& IsNotNull location $&& IsNotNull source $&& IsNotNull region
                       }
        theResponse @?= [[("source",RSodaText "nn"),("place",RPoint (Point {longitude = -117.6778, latitude = 36.9447})),("area",RSodaText "northern california")],[("source",RSodaText "nn"),("place",RPoint (Point {longitude = -117.6903, latitude = 36.9417})),("area",RSodaText "central california")],[("source",RSodaText "pr"),("place",RPoint (Point {longitude = -64.0849, latitude = 19.7859})),("area",RSodaText "north of the virgin islands")]]
    -- I suppose I'm not really testing individual units in some of these "unit" tests.
    , testCase "Testing other SODA functions, operators, and values." $ do
        theResponse <- getSodaResponse testDomain testDataset $
            emptyQuery { selects = Just [ Alias (Case [(Expr $ sodaM True $&& sodaM False, sodaE "foo"), (Expr $ number_of_stations $> (sn 15.0 $- sn 5.0), sodaE "bar"), (Expr $ depth $== sn 0.0, sodaE "baz")]) "case_result" ]
                       , limit = Just 3
                       , wheres = Just . Where $
                            IsNotNull depth
                            $&& IsNotNull number_of_stations 
                             $&& WithinCircle location (sn 63) (sn (-147.0)) (sn 60000)
                       }
        theResponse @?= [[("case_result",RSodaText "bar")],[("case_result",RSodaText "bar")],[("case_result",RSodaText "bar")]]
    , testCase "Testing out aggregate related functionality SODA functions, operators, and values." $ do
        theResponse <- getSodaResponse testDomain testDataset $
            emptyQuery { selects = Just [ Alias (Avg magnitude) "avg_mag" ]
                       , limit = Just 3
                       , groups = Just [Group magnitude]
                       , having = Just . Having $ ((Column "avg_mag") :: Column SodaNum) $> sn 1.0-- Points out that aliases need to be improved.
                       }
        theResponse @?= [[("avg_mag",RSodaNum (SodaNum {getSodaNum = 1.01}))],[("avg_mag",RSodaNum (SodaNum {getSodaNum = 1.02}))],[("avg_mag",RSodaNum (SodaNum {getSodaNum = 1.03}))]]
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
        sn = SodaVal . SodaNum
        sodaE = Expr . SodaVal
        sodaM = SodaVal . Just

source             = Column "source"             :: Column SodaText
earthquake_id      = Column "source"             :: Column SodaText
version            = Column "source"             :: Column SodaText
magnitude          = Column "magnitude"          :: Column SodaNum
depth              = Column "depth"              :: Column SodaNum
number_of_stations = Column "number_of_stations" :: Column SodaNum
region             = Column "region"             :: Column SodaText
location_city      = Column "location_city"      :: Column SodaText
location           = Column "location"           :: Column Point
location_address   = Column "location_address"   :: Column SodaText
location_zip       = Column "location_zip"       :: Column SodaText
location_state     = Column "location_state"     :: Column SodaText
