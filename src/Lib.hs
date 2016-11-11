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
-
- Mention that I'd also potentially like to make a servant version, but that I would have to learn more about type-level programming.

todo:
- Create consumer API - in process
- Create producer API
- Add authentication and API token abilities.
- (Look at other SODA libraries to get ideas)
- Have it be able receive different files types using the right types.

 -}

type Endpoint = String
type Request = String
type Query = String
type Domain = String
type DatasetID = String
-- Might want to make this a simpler type since the formats are currently limited.
type ResponseFormat = String

runRawRequest :: Request -> IO L8.ByteString
runRawRequest request = do
    request <- Http.parseRequest request
    response <- Http.httpLBS request
    return $ Http.getResponseBody response

runRequest :: Domain -> DatasetID -> ResponseFormat -> Query -> IO L8.ByteString
runRequest domain datasetID format query= runRawRequest $ urlBuilder domain datasetID format query

urlBuilder :: Domain -> DatasetID -> ResponseFormat -> Query -> Request
urlBuilder domain datasetID format query = "https://" ++ domain ++ "/resource/" ++ datasetID ++ format ++ query

testRequest :: Request
testRequest = "https://" ++ testDomain ++ "/resource/" ++ testDataset ++ testResponse ++ testQuery

someFunc :: IO ()
someFunc = do
    response <- runRequest testDomain testDataset testResponse testQuery
    L8.putStrLn response
    -- I'll probably need to use the status code and response header later
    --putStrLn $ "The status code was: " ++
    --           show (Http.getResponseStatusCode response)
    --print $ Http.getResponseHeader "Content-Type" response

-- Test stuff
-- I know I should just make actual tests, but I'm new to this so I'll do that later.

testDomain :: Domain
testDomain = "open.whitehouse.gov"

testDataset :: DatasetID
testDataset = "i7dt-eubi"

testResponse :: ResponseFormat
testResponse = ".json"

testQuery :: Query
testQuery = "?name=Drain,%20Gershwin%20A."
