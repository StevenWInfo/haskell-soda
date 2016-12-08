{-# LANGUAGE OverloadedStrings #-}
module Soda
    ( ResponseFormat (..)
    , formatToUrl
    , Domain
    , DatasetID
    , RawParameters
    , urlBuilder
    , runRequest
    ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import System.IO
import Data.Text (Text, pack, append)
import Data.Monoid ((<>))
import Network.HTTP.Req
import Control.Exception

import Query

{-
#Notes

- Might want to make parts more complex than type synonyms.

- Might want to provide a way to replace any non-string piece of the query with a string just to make it more flexible
    - Also might want to be able to return it in several data types as well.

- Strings are the obvious type, Text is the more commonly recommended, and Bytestring is what http-conduit returns.

- Mention that I'd also potentially like to make a servant version, but that I would have to learn more about type-level programming.

- Have ways to change the return string format as well as whether it is given as a string type or a more native format.
    - Should it just give it as a string and the user parses it into a native type/format or should I include things to automatically convert it?
    - Maybe I'll put it in eventually, but for now just return as string.

- Perhaps, try out that new REST library.

##todo

- Create consumer API - in process
- Create producer API
- Add authentication and API token abilities.
- (Look at other SODA libraries to get ideas)
- Have it be able receive different files types using the right types.
- Eventually take advantage of http manager and connections.
- Handle metadata and other data
- Handle error responses
- Have some way to run raw queries and turn complete or partial EDSL queries into raw queries.
- Not sure if I want to use String, Text, Bytestring, etc. Decide eventually.

 -}

type Endpoint = String
type Request = String
type RawParameters = [(Text, String)]
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

instance MonadHttp IO where
  handleHttpException = throwIO

runRequest :: Domain -> DatasetID -> ResponseFormat -> RawParameters -> IO String
runRequest domain datasetID format query = do
    let url = urlBuilder domain datasetID format
    let param = foldr1 (<>) $ map (\(x,y) -> x =: y) query
    response <- req GET url NoReqBody lbsResponse param
    return (L8.unpack (responseBody response))

-- Eventually change RawParameters to Query type
--urlBuilder :: Domain -> DatasetID -> ResponseFormat -> RawParameters -> Url Https
urlBuilder domain datasetID format = https domain' /: "resource" /: (datasetID' `append` "." `append` format')
    where domain' = pack domain
          datasetID' = pack datasetID
          format' = pack (formatToUrl format)
