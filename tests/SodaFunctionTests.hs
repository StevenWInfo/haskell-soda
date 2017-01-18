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

-- The double whammy of having to put in SodaVal and the Double type decleration is really frustrating.
-- I suppose I could have SodaVal NOT be a GADT and have seperate constructors for all of them. Then I think users could have overloadedStrings I think and I don't think a type decleration for double would be required. I could also get rid of the newtypes for Number and Money and just have different data constructors. I could get rid of the SodaType typeclass which would simplify things and have a single toUrlPart function rather than seperated instances. It might also be nice in a way because then it is quite visible and explicit what types users are putting in (although not having to indicate the type would probably be nicer).

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
    -- I'm currently expecting this to fail because my implementation takes advantage of the Point type rather than two numbers.
    -- Well, I appearantly shouldn't try and be clever with the point type, because you could put two number columns for latitude and longitude which my API would prevent.
    , testCase "Troublesome within_circle function." $
        (toUrlParam $ WithinCircle loc (sn 45.23) (sn 55.8) (sn 45.2)) @?= "within_circle(some_place, 45.23, 55.8, 45.2)"
    , testCase "Testing addition with different numeric types." $
        (toUrlParam $ (sn 5.1 $+ (sv $ Money 7.20) $+ dblCol)) @?= "5.1 + '7.2' + some_double"
    ]
    where sv :: (SodaType a) => a -> SodaVal a -- Note to self: GADTs are finicky
          sv     = SodaVal
          sd :: Double -> SodaVal Double -- Note to self: GADTs are *very* finicky
          sd a   = sv a
          sn n   = sv $ SodaNum n -- Maybe should just make a different version of SodaVal for all numbers.
          pt a b = sv $ Point { longitude                                                                  = a, latitude = b }
          loc    = Column "some_place" :: Column Point
          dblCol = Column "some_double" :: Column Double
