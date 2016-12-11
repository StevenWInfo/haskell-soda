{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Soda
    ( ResponseFormat (..)
    , formatToUrl
    , Domain
    , DatasetID
    , RawParameters
    , urlBuilder
    , getStringResponse
    , getStringBody
    --, getJsonResponse
    ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import System.IO
import Data.Text (Text, pack, append)
import Data.Monoid ((<>), Monoid)
--import GHC.Generics
--import Data.Aeson
import Network.HTTP.Req
import Control.Exception

import Query

{- #Notes

- Might want to make parts more complex than type synonyms.

- Might want to provide a way to replace any non-string piece of the query with a string just to make it more flexible
    - Also might want to be able to return it in several data types as well.

- Mention that I'd also potentially like to make a servant version, but that I would have to learn more about type-level programming.

- Have ways to change the return string format as well as whether it is given as a string type or a more native format.
    - Should it just give it as a string and the user parses it into a native type/format or should I include things to automatically convert it?
    - Maybe I'll put it in eventually, but for now just return as string.

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
- Figure out how to get info out in JSON, XML, and other types. JSON was a little more complicated than I had expected.
 -}

type Endpoint      = String
type Request       = String
type RawParameters = [(String, String)]
type Domain        = String
type DatasetID     = String

-- Have this affect the mime type?
data ResponseFormat = CSV | GeoJSON | JSON | RDFXML | XML

-- Should it maybe put the dot in front? Perhaps make it part of Show?
formatToUrl :: ResponseFormat -> String
formatToUrl CSV     = "csv"
formatToUrl GeoJSON = "geojson"
formatToUrl JSON    = "json"
formatToUrl RDFXML  = "rdf"
formatToUrl XML     = "xml"

instance MonadHttp IO where
  handleHttpException = throwIO

-- Misnamed right now because the output is an LbsResponse
-- Maybe should accept a request record type.
--getStringResponse :: Domain -> DatasetID -> ResponseFormat -> RawParameters -> IO String
getStringResponse domain datasetID format query = do
    let url = urlBuilder domain datasetID format
    let param = foldr1 (<>) $ map (\(x,y) -> (pack x) =: y) query
    response <- req GET url NoReqBody lbsResponse param
    return response

getStringBody :: Domain -> DatasetID -> ResponseFormat -> RawParameters -> IO String
getStringBody domain datasetID format query = (getStringResponse domain datasetID format query) >>= (return . L8.unpack . responseBody)

{-
-- Perhaps figuring out how to return different types based on other parameters would be better.
--getJsonResponse :: Domain -> DatasetID -> ResponseFormat -> RawParameters -> IO a
getJsonResponse domain datasetID format query = do
    response <- req GET url NoReqBody jsonResponse param
    return $ responseBody response
    where url   = urlBuilder domain datasetID format
          param = foldr1 (<>) $ map (\(x,y) -> (pack x) =: y) query
          -}

--urlBuilder :: Domain -> DatasetID -> ResponseFormat -> Url Https
urlBuilder domain datasetID format = https domain' /: "resource" /: (datasetID' `append` "." `append` format')
    where domain' = pack domain
          datasetID' = pack datasetID
          format' = pack (formatToUrl format)
