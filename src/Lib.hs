{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Network.HTTP.Simple as Http

import Query

{-
Notes:
- Might want to make parts more complex than type synonyms.

- Might want to provide a way to replace any non-string piece of the query with a string just to make it more flexible
-   Also might want to be able to return it in several data types as well.
-
- Strings are the obvious type, Text is the more commonly recommended, and Bytestring is what http-conduit returns.
-
- Mention that I'd also potentially like to make a servant version, but that I would have to learn more about type-level programming.
-
- Have ways to change the return string format as well as whether it is given as a string type or a more native format.
-   Should it just give it as a string and the user parses it into a native type/format or should I include things to automatically convert it?
-   Maybe I'll put it in eventually, but for now just return as string.

- I realized something obvious: The logic for the language is the same as for a language/parser except the lexical analysis done already.

todo:
- Create consumer API - in process
- Create producer API
- Add authentication and API token abilities.
- (Look at other SODA libraries to get ideas)
- Have it be able receive different files types using the right types.
- Eventually take advantage of http manager and connections.
- Handle metadata and other data
- Handle error responses

 -}

type Endpoint = String
type Request = String
type TQuery = String
type Domain = String
type DatasetID = String

-- Have this affect the mime type?
data ResponseFormat = CSV | GeoJSON | JSON | RDFXML | XML

-- Should it maybe put the dot in front? Perhaps make it part of a typeclass instead?
formatToUrl :: ResponseFormat -> String
formatToUrl CSV = "csv"
formatToUrl GeoJSON = "geojson"
formatToUrl JSON = "json"
formatToUrl RDFXML = "rdf"
formatToUrl XML = "xml"

runRawRequest :: Request -> IO L8.ByteString
runRawRequest request = do
    request <- Http.parseRequest request
    response <- Http.httpLBS request
    return $ Http.getResponseBody response

runRequest :: Domain -> DatasetID -> ResponseFormat -> TQuery -> IO L8.ByteString
runRequest domain datasetID format query= runRawRequest $ urlBuilder domain datasetID format query

urlBuilder :: Domain -> DatasetID -> ResponseFormat -> TQuery -> Request
urlBuilder domain datasetID format query = "https://" ++ domain ++ "/resource/" ++ datasetID ++ "." ++ (formatToUrl format) ++ query

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
testResponse = CSV

testQuery :: TQuery
testQuery = "?name=Drain,%20Gershwin%20A."
