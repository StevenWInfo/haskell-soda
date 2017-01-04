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
-- I suppose I could have SodaVal NOT be a GADT and have seperate constructors for all of them. Then I think users could have overloadedStrings I think and I don't think a type decleration for double would be required. I could also get rid of the newtypes for Number and Money and just have different data constructors. I could get rid of the SodaTypes typeclass which would simplify things and have a single toUrlPart function rather than seperated instances. It might also be nice in a way because then it is quite visible and explicit what types users are putting in (although not having to indicate the type would probably be nicer).

tests :: TestTree
tests = testGroup "Soda Function Tests"
    [ testCase "Basic SodaFumc test with Not()" $
        (toUrlParam . Not . SodaVal $ Just True) @?= "Not true"
    , testCase "Basic SodaFunc test with Between" $
        (toUrlParam $ Between (SodaVal (3.0 :: Double)) (SodaVal (2.0 :: Double)) (SodaVal (5.0 :: Double))) @?= "3.0 between 2.0 and 5.0"
    ]
    --where SVal = SodaVal
