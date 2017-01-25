module SodaFunctionTests
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
tests = testGroup "Soda Function Tests"
    [ testCase "Basic SodaFumc test with Not()" $
        (toUrlParam . Not . SodaVal $ Just True) @?= "NOT true"
    , testCase "Basic SodaFunc test with Between" $
        (toUrlParam $ Between (SodaVal (3.0 :: Double)) (SodaVal (2.0 :: Double)) (SodaVal (5.0 :: Double))) @?= "3.0 between 2.0 and 5.0"
    , testCase "Function within a function" $
        (toUrlParam $ Lower (Upper (sv "First upper, then lower."))) @?= "lower(upper('First upper, then lower.'))"
    , testCase "Distance function test" $
        (toUrlParam $ Distance loc (pt (-1.3) 27.0)) @?= "distance_in_meters(some_place, 'POINT (-1.3 27.0)')"
    , testCase "In function test without double type decleration" $
        (toUrlParam $ In (sd 5.6) [Expr ((sd 4.3) $- (sd 7.8)), Expr ((sd (-2.8)) $+ dblCol), Expr (sd 4.0)]) @?= "5.6 IN(4.3 - 7.8, -2.8 + some_double, 4.0)"
    , testCase "Troublesome within_circle function." $
        (toUrlParam $ WithinCircle loc (sn 45.23) (sn 55.8) (sn 45.2)) @?= "within_circle(some_place, 45.23, 55.8, 45.2)"
    , testCase "Testing addition with different numeric types." $
        (toUrlParam $ (sn 5.1 $+ (sv $ Money 7.20) $+ dblCol)) @?= "5.1 + '7.2' + some_double"
    , testCase "Testing addition with different numeric types." $
        (toUrlParam $ (sn 5.1 $+ (sv $ Money 7.20) $+ dblCol)) @?= "5.1 + '7.2' + some_double"
    , testCase "Testing an example that's in the documentation." $
        (toUrlParam $ Between (locationDistance $* sn 3) (sn 7) (sn 58 $+ sn 3)) @?= "distance_in_meters(location, 'POINT (45.3 -87.2)') * 3.0 between 7.0 and 58.0 + 3.0"
    ]
    where sv :: (SodaType a) => a -> SodaVal a -- Note to self: GADTs are finicky
          sv     = SodaVal
          sd :: Double -> SodaVal Double -- Note to self: GADTs are *very* finicky
          sd a   = sv a
          sn n   = sv $ SodaNum n
          pt a b = sv $ Point { longitude = a, latitude = b }
          loc    = Column "some_place" :: Column Point
          dblCol = Column "some_double" :: Column Double
          locationDistance = Distance location $ SodaVal (Point 45.3 (-87.2) )
          location = Column "location" :: Column Point
