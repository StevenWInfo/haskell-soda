
module SodaTests
    ( tests
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Aeson
import Data.Maybe (fromJust, fromMaybe)
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
    , testCase "Testing full API call" $ do
        theResponse <- getSodaResponse testDomain testDataset $ emptyQuery { filters = Just [ (Column "magnitude" :: Column SodaNum) $= (SodaVal $ SodaNum 1.6) ], limit = Just 1 }
        theResponse @?= [[("source",RSodaText "ak"),("region",RSodaText "36km W of Valdez, Alaska"),("number_of_stations",RSodaNum (SodaNum {getSodaNum = 6.0})),("magnitude",RSodaNum (SodaNum {getSodaNum = 1.6})),("earthquake_id",RSodaText "ak11243041"),("depth",RSodaNum (SodaNum {getSodaNum = 0.0}))]]
    -- Any value for this is fine as long as the response code is 200.
    , testCase "Testing special characters. (Any value is fine)" $ do
        theResponse <- getSodaResponse testDomain testDataset $ emptyQuery { filters = Just [ (Column "region" :: Column SodaText) $= (SodaVal "a!@#$%^&*(),.;:\"'?+=-_[]{}~`<>\\| ") ], limit = Just 1 }
        theResponse @?= []
    , testCase "Testing the query on the SODA doc home page" $ do
        theResponse <- getSodaResponse "data.ct.gov" "y6p2-px98" $
            emptyQuery { filters = Just [ (Column "category" :: Column SodaText) $= SodaVal "Fruit", (Column "item" :: Column SodaText) $= SodaVal "Peaches"]
                       , limit = Just 3
                       }
        theResponse @?= [[("zipcode",RSodaText "06791"),("location_1_state",RSodaText "CT"),("location_1_location",RSodaText "16 Bogue Rd"),("location_1_city",RSodaText "Harwinton"),("location_1",RPoint (Point {longitude = -73.09627264999966, latitude = 41.77989387000048})),("l",RSodaNum (SodaNum {getSodaNum = 0.0})),("item",RSodaText "Peaches"),("farmer_id",RSodaNum (SodaNum {getSodaNum = 3402.0})),("category",RSodaText "Fruit"),("business",RSodaText "Francis Motuzick Jr")],[("zipcode",RSodaText "06759"),("phone1",RSodaText "860-361-6216"),("location_1_state",RSodaText "CT"),("location_1_location",RSodaText "403 Beach Street"),("location_1_city",RSodaText "Litchfield"),("location_1",RPoint (Point {longitude = -73.22418539499967, latitude = 41.7874849100005})),("l",RSodaNum (SodaNum {getSodaNum = 18.0})),("item",RSodaText "Peaches"),("farmer_id",RSodaNum (SodaNum {getSodaNum = 16352.0})),("farm_name",RSodaText "Morning Song Farms"),("category",RSodaText "Fruit"),("business",RSodaText "Morning Song Farms")],[("zipcode",RSodaText "06477"),("phone1",RSodaText "203-795-0571"),("location_1_state",RSodaText "CT"),("location_1_location",RSodaText "707 Derby Turnpike"),("location_1_city",RSodaText "Orange"),("location_1",RPoint (Point {longitude = -73.04627981099964, latitude = 41.31108858500045})),("l",RSodaNum (SodaNum {getSodaNum = 15.0})),("item",RSodaText "Peaches"),("farmer_id",RSodaNum (SodaNum {getSodaNum = 6640.0})),("farm_name",RSodaText "Field View Farm"),("category",RSodaText "Fruit"),("business",RSodaText "Field View Farm")]]
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
    , testCase "One of the examples I'll have in the README" $ do
        theResponse <- getSodaResponse testDomain testDataset $
            emptyQuery { selects = Just [ Select magnitude, Alias (region $++ SodaVal " " $++ source) "region_and_source"]
                       , wheres  = Just . Where $
                            IsNotNull region 
                            $&& IsNotNull source 
                            $&& IsNotNull location 
                            $&& WithinCircle location (sn 63) (sn (-147.0)) (sn 60000)
                       , orders  = Just $ [Order magnitude ASC]
                       , limit   = Just 3
                       }
        let mag = (safeHead theResponse >>= lookup "magnitude" >>= checkNum)
        mag @?= Just 0.3
        let regSource = (safeHead theResponse >>= lookup "region_and_source" >>= checkText)
        regSource @?= Just "82km E of Cantwell, Alaska ak"
        let stringMag = mag >>= (Just . (\mag -> mag ++ " magnitude ") . show)
        flip (@?=) "0.3 magnitude 82km E of Cantwell, Alaska ak" $ case ((++) <$> stringMag <*> regSource) of
            Nothing -> "Nothing here"
            Just x  -> x
        let mags = map (\x -> lookup "magnitude" x >>= checkNum >>= (Just . (\mag -> mag ++ " magnitude ") . show)) theResponse
        let regSources = map (\x -> lookup "region_and_source" x >>= checkText) theResponse
        zipWith (\mag rs -> fromMaybe "Nothing here" $ (++) <$> mag <*> rs) mags regSources @?= ["0.3 magnitude 82km E of Cantwell, Alaska ak", "0.6 magnitude 64km E of Cantwell, Alaska ak", "0.8 magnitude 73km SSW of Delta Junction, Alaska ak"]
        theResponse @?= [[("region_and_source",RSodaText "82km E of Cantwell, Alaska ak"),("magnitude",RSodaNum (SodaNum {getSodaNum = 0.3}))],[("region_and_source",RSodaText "64km E of Cantwell, Alaska ak"),("magnitude",RSodaNum (SodaNum {getSodaNum = 0.6}))],[("region_and_source",RSodaText "73km SSW of Delta Junction, Alaska ak"),("magnitude",RSodaNum (SodaNum {getSodaNum = 0.8}))]]
    , testCase "Testing more complicated query creation" $ do
        firstResponse <- getSodaResponse testDomain  testDataset $
            emptyQuery { selects = Just [ Select location, Select region]
                       , wheres  = Just . Where $ IsNotNull location $&& IsNotNull magnitude
                       , orders  = Just $ [ Order magnitude DESC ]
                       , limit   = Just 1
                       }
        let maxLocation = safeHead firstResponse >>= lookup "location" >>= checkPoint
        maxLocation @?= Just (Point {longitude = 144.8994, latitude = 6.5092})
        secondResponse <- case maxLocation of
            Nothing       -> return []
            Just maxPoint -> getSodaResponse "odn.data.socrata.com"  "h7w8-g2pa" $
                                emptyQuery { selects = Just $ (Select geoid) : inputSelect 
                                           , wheres  = Just . Where $
                                                Intersects (Column "the_geom" :: Column MultiPolygon) (SodaVal maxPoint)
                                                $|| WithinCircle (Column "the_geom" :: Column MultiPolygon) (sn $ latitude maxPoint) (sn $ longitude maxPoint) (sn $ 1000000)
                                           , limit   = Just 1
                                           }
        secondResponse @?= [[("name",RSodaText "San Jose (Oleai)"),("geoid",RSodaText "6947205")]]
    ]
    where
        testDomain  = "soda.demo.socrata.com"
        testDataset = "6yvf-kk3n"
        testFormat  = JSON
        testQuery   = [("$where", "earthquake_id in('ak11243041', 'ak11246151', 'ak11246918')")]
        result      = "[{\"depth\":\"0\",\"earthquake_id\":\"ak11243041\",\"magnitude\":\"1.6\",\"number_of_stations\":\"6\",\"region\":\"36km W of Valdez, Alaska\",\"source\":\"ak\"}]\n"
        selectNotFound :: HttpException -> Maybe Status.Status
        selectNotFound (VanillaHttpException (Http.HttpExceptionRequest _ (Http.StatusCodeException (rec) _))) = Just (Http.responseStatus rec)
        selectNotFound _           = Nothing
        sn                         = SodaVal . SodaNum
        sodaE                      = Expr . SodaVal
        sodaM                      = SodaVal . Just
        safeHead []                = Nothing
        safeHead (x:xs)            = Just x
        checkNum (RSodaNum num)    = Just (getSodaNum num)
        checkNum _                 = Nothing
        checkText (RSodaText text) = Just text
        checkText _                = Nothing
        checkPoint (RPoint point)  = Just point
        checkPoint _               = Nothing
        inputSelect                = [ Select (Column "name" :: Column SodaText) ]
        geoid                      = Column "geoid"     :: Column SodaText

source             = Column "source"             :: Column SodaText
earthquake_id      = Column "earthquake_id"      :: Column SodaText
version            = Column "version"            :: Column SodaText
magnitude          = Column "magnitude"          :: Column SodaNum
depth              = Column "depth"              :: Column SodaNum
number_of_stations = Column "number_of_stations" :: Column SodaNum
region             = Column "region"             :: Column SodaText
location_city      = Column "location_city"      :: Column SodaText
location           = Column "location"           :: Column Point
location_address   = Column "location_address"   :: Column SodaText
location_zip       = Column "location_zip"       :: Column SodaText
location_state     = Column "location_state"     :: Column SodaText
