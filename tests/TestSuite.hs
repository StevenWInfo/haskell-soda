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

instance MonadHttp IO where
  handleHttpException = throwIO

main :: IO ()
main = do
    lbs <- req GET (http "jsonplaceholder.typicode.com" /: "posts" /: "1") NoReqBody lbsResponse mempty
    putStrLn (L8.unpack (responseBody lbs))
    defaultMain tests

tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
    [ testCase "urlBuilder smoke test" $
        urlBuilder testDomain testDataset testFormat testQuery @?= "https://soda.demo.socrata.com/resource/4tka-6guv.json?$where=magnitude > 3.0"
    -- This isn't a great test because it will fail if the dataset changes, but it's useful now.
    , testCase "Try out executing query." $ do
        contents <- readFile "tests/data/4tka-6guvMag3.json"
        response <- runRequest testDomain testDataset testFormat testQuery
        L8.unpack response @?= (init contents) -- I haven't tested this yet.
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
testQuery = "?$where=magnitude > 3.0"
