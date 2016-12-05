import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Soda

main :: IO ()
--main = putStrLn "Test suite not yet implemented"
main = defaultMain tests

tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
    [ testCase "Testing unit tests" $
        assertEqual "Some text" "Hollywood" ("Holly" ++ "wood")
    , testCase "Test testCase" $
        (formatToUrl CSV) @?= "csv"
    ]

{-
someFunc :: IO ()
someFunc = do
    response <- runRequest testDomain testDataset testResponse testQuery
    L8.putStrLn response
    -- I'll probably need to use the status code and response header later
    --putStrLn $ "The status code was: " ++
    --           show (Http.getResponseStatusCode response)
    --print $ Http.getResponseHeader "Content-Type" response


testDomain :: Domain
testDomain = "open.whitehouse.gov"

testDataset :: DatasetID
testDataset = "i7dt-eubi"

testResponse :: ResponseFormat
testResponse = CSV

testQuery :: RawParameters
testQuery = "?name=Drain,%20Gershwin%20A."
-}
