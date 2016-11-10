{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Network.HTTP.Simple as Http

{-
Notes:

- Might want to make parts more complex than type synonyms.

- Might want to provide a way to replace any non-string piece of the query with a string just to make it more flexible
-   Also might want to be able to return it in several data types as well.
-
- Strings are the obvious type, Text is the more commonly recommended, and Bytestring is what http-conduit returns.
 -}

type Endpoint = String
type Request = String
type Query = String
type Domain = String
type DatasetID = String
-- Might want to make this a simpler type since the formats are currently limited.
type ResponseFormat = String

runRequest :: Request -> IO L8.ByteString
runRequest request = do
    request <- Http.parseRequest request
    response <- Http.httpLBS request
    return $ Http.getResponseBody response

someFunc :: IO ()
someFunc = do
    response <- runRequest testRequest
    L8.putStrLn response
    -- I'll probably need to use the status code and response header later
    --putStrLn $ "The status code was: " ++
    --           show (Http.getResponseStatusCode response)
    --print $ Http.getResponseHeader "Content-Type" response

-- Test stuff

testDomain :: Domain
testDomain = "open.whitehouse.gov"

testDataset :: DatasetID
testDataset = "i7dt-eubi"

testResponse :: ResponseFormat
testResponse = ".json"

testQuery :: Query
testQuery = "?name=Drain,%20Gershwin%20A."

testRequest :: Request
testRequest = "https://" ++ testDomain ++ "/resource/" ++ testDataset ++ testResponse ++ testQuery
