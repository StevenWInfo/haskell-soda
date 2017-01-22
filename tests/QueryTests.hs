module QueryTests
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
    [ testCase "Smoke test for query" $
        queryToString emptyQuery @?= ""
    , testCase "Basic filter query test" $
        (queryToString $ emptyQuery { filters = Just [ colFoo $= (SodaVal "Bar") ]}) @?= "Foo='Bar'"
    , testCase "Filter query test with a Soda Function" $
        (queryToString $ emptyQuery { filters = Just [ numCol $= avgCol ]}) @?= "Num=avg(Baz)"
    , testCase "Testing select parameter building" $
        (queryToString $ emptyQuery { selects = Just [ Select colFoo, Alias numCol "NumAlias" ]}) @?= "$select=Foo, Num AS NumAlias"
    , testCase "Testing group parameter building" $
        (queryToString $ emptyQuery { groups = Just [ Group colFoo, Group numCol ]}) @?= "$group=Foo, Num"
    , testCase "Testing order parameter building" $
        (queryToString $ emptyQuery { orders = Just [ Order colFoo ASC, Order numCol DESC ]}) @?= "$order=Foo ASC, Num DESC"
    , testCase "Testing limit parameter building" $
        (queryToString $ emptyQuery { limit = Just 4}) @?= "$limit=4"
    , testCase "Testing offset parameter building" $
        (queryToString $ emptyQuery { offset = Just 6}) @?= "$offset=6"
    , testCase "Testing search parameter building" $
        (queryToString $ emptyQuery { search = Just "Lorem Ipsum"}) @?= "$q=Lorem Ipsum"
    , testCase "Testing BOM parameter building" $
        (queryToString $ emptyQuery { bom = Just True}) @?= "$$bom=true"
    , testCase "Testing all query parts" $
        (queryToString 
            $ emptyQuery { filters = Just [ numCol $= (SodaVal (SodaNum 5.0))]
                         , selects = Just [ Select numCol, Alias avgCol "average"]
                         , groups  = Just [ Group colFoo ]
                         , orders  = Just [ Order numCol ASC ]
                         , offset  = Just 5
                         , limit   = Just 3
                         , search  = Just "Hello, world"
                         , bom     = Just False
                         }
        ) @?= "Num=5.0&$select=Num, avg(Baz) AS average&$order=Num ASC&$group=Foo&$limit=3&$offset=5&$q=Hello, world&$$bom=false"
    -- I suppose I should be 100% sure it will always return the same values, which will probably mean specifying primary key values. If this test breaks, then specify primary key values.
    , testCase "Testing a query I was getting strange results for" $
        (queryToString $
            emptyQuery { selects = Just $ [ Select source, Alias location "place", Alias (Upper location_state) "state" ]
                       , limit = Just 3
                       , wheres = Just . Where $ number_of_stations $> sn 1.0 $&& IsNotNull location $&& IsNotNull location_state
                       }
        ) @?= "$select=source, location AS place, upper(location_state) AS state&$where=number_of_stations > 1.0 AND location IS NOT NULL AND location_state IS NOT NULL&$limit=3"
    , testCase "Testing another query I was getting strange results for" $
        (queryToString $
            emptyQuery { selects = Just [ Alias (Case [(Expr $ sodaM True $&& sodaM True, sodaE "foo"), (Expr $ number_of_stations $> (sn 15.0 $- sn 5.0), sodaE "bar"), (Expr $ depth $== sn 0.0, sodaE "baz")]) "case_result" ]
                       , limit = Just 3
                       , wheres = Just . Where $
                            IsNotNull depth
                            $&& IsNotNull number_of_stations 
                            $&& WithinCircle location (sn (-147.0)) (sn 63) (sn 100)
                       }
        ) @?= "$select=case(true AND true, 'foo', number_of_stations > 15.0 - 5.0, 'bar', depth = 0.0, 'baz') AS case_result&$where=depth IS NOT NULL AND number_of_stations IS NOT NULL AND within_circle(location, -147.0, 63.0, 100.0)&$limit=3"
    , testCase "A query that I can't get right manually." $
        (queryToString $
            emptyQuery { selects = Just [ Select magnitude, Alias (region $++ SodaVal " " $++ source) "region_and_source"]
                       , wheres  = Just . Where $
                            IsNotNull region 
                            $&& IsNotNull source 
                            $&& IsNotNull location 
                            $&& WithinCircle location (sn 63) (sn (-147.0)) (sn 60000)
                       , orders  = Just $ [Order magnitude ASC]
                       , limit   = Just 3
                       }
        ) @?= "$select=magnitude, region || ' ' || source AS region_and_source&$where=region IS NOT NULL AND source IS NOT NULL AND location IS NOT NULL AND within_circle(location, 63.0, -147.0, 60000.0)&$order=magnitude ASC&$limit=3"
    -- Might want to move these tests to another file.
    , testCase "Testing getting values out of responses" $
        getVal ((Expr $ SodaVal "Foobar") :: Expr String) @?= Right "Foobar"
    , testCase "Testing getting error for incorrect input to getVal" $
        getVal (Expr colFoo) @?= Left BadLower
    ]
    where colFoo = Column "Foo" :: Column SodaText
          numCol = Column "Num" :: Column SodaNum
          avgCol = Avg ((Column "Baz") :: Column SodaNum)
          sn     = SodaVal . SodaNum
          sodaE  = Expr . SodaVal
          sodaM  = SodaVal . Just

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
