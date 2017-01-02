{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
    Module      : Soda
    Description : Sending and Receiving queries.
    Copyright   : (c) Steven W
    Maintainer  : Steven W <StevenW.Info@gmail.com>
    Stability   : Unstable

The module for sending off queries and getting their results.
-}

module Soda
    ( ResponseFormat (..)
    , Domain
    , DatasetID
    , RawParameters
    , urlBuilder
    , getStringResponse
    , getStringBody
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

{-
#Notes

- Might want to provide a way to replace any non-string piece of the query with a string just to make it more flexible
    - Also might want to be able to return it in several data types as well.

- (Look at other SODA libraries to get ideas)

- May want to rename this to network or something.

##todo

- Handle metadata and other data
- Have some way to run raw queries and turn complete or partial EDSL queries into raw queries.
-}

-- Not sure if I should even export this.
-- |The type that specifies what the parameters are in the request URL. I've made it public so that you can make any call to a SODA, but it isn't recommended. Skipping over using the Query type gets rid of a lot of the compile time guarantees provided by this library.
type RawParameters = [(String, String)]

-- |Specifies what the domain of a request URL should be.
type Domain        = String
-- |Used for specifying the ID of the dataset that you want to query.
type DatasetID     = String

-- Have this affect the mime type?
-- |The type which specifies in the request, the format of the response.
data ResponseFormat = CSV | GeoJSON | JSON | RDFXML | XML

formatToUrl :: ResponseFormat -> String
formatToUrl CSV     = "csv"
formatToUrl GeoJSON = "geojson"
formatToUrl JSON    = "json"
formatToUrl RDFXML  = "rdf"
formatToUrl XML     = "xml"

instance MonadHttp IO where
  handleHttpException = throwIO

-- For the function which interprets directly into Haskell values, we don't need the response format since it won't affect what they get. We can always just use JSON or something.

-- Misnamed right now because the output is an LbsResponse
-- Also, probably don't need to use do notation.
-- getStringResponse :: Domain -> DatasetID -> ResponseFormat -> RawParameters -> IO String
-- |Gets the whole response. Misnamed right now because it is actually lazy byte strings, and it's returning a response data structure.
getStringResponse domain datasetID format query = do
    let url = urlBuilder domain datasetID format
    let param = foldr1 (<>) $ map (\(x,y) -> (pack x) =: y) query
    response <- req GET url NoReqBody lbsResponse param
    return response

-- (should probably take just the Query type)
-- |Gets the body of a response from a query given the Domain, DatasetID, ResponseFormat, and query parameters as a list of tuples.
getStringBody :: Domain -> DatasetID -> ResponseFormat -> RawParameters -> IO String
getStringBody domain datasetID format query = (getStringResponse domain datasetID format query) >>= (return . L8.unpack . responseBody)

-- Maybe this should also take the parameters.
--urlBuilder :: Domain -> DatasetID -> ResponseFormat -> Url Https
-- |Builds the non-parameter part of the URL out of the Domain, DatasetID, and ResponseFormat.
urlBuilder domain datasetID format = https domain' /: "resource" /: (datasetID' `append` "." `append` format')
    where domain' = pack domain
          datasetID' = pack datasetID
          format' = pack (formatToUrl format)

-- Create a string URL builder.
