{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Soda

import System.IO
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text (Text)
import Network.HTTP.Req
import Control.Exception

{-
 - Break this into multiple files soon.
 -}

main :: IO ()
main = do
    defaultMain tests

tests = testGroup "Tests" [unitTests]

-- The test that actually queries isn't a great test because it will fail if the dataset changes, but it's useful now.
unitTests = testGroup "Unit tests"
    [ testCase "Try out executing query." $ do
        contents <- readFile "tests/data/4tka-6guvMag3.json"
        response <- runRequest testDomain testDataset testFormat testQuery
        response @?= (init contents) -- I haven't tested this yet.
    --, testCase "urlBuilder smoke test" $
        --urlBuilder testDomain testDataset testFormat testQuery @?= "https://soda.demo.socrata.com/resource/4tka-6guv.json?$where=magnitude > 3.0"
    ]

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
