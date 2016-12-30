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

- Might want to provide a way to replace any non-string piece of the query with a string just to make it more flexible
    - Also might want to be able to return it in several data types as well.

- (Look at other SODA libraries to get ideas)

- May want to rename this to network or something.

##todo

- Handle metadata and other data
- Have some way to run raw queries and turn complete or partial EDSL queries into raw queries.
 -}

type Endpoint      = String
type Request       = String
type RawParameters = [(String, String)]
type Domain        = String
type DatasetID     = String

-- Have this affect the mime type?
data ResponseFormat = CSV | GeoJSON | JSON | RDFXML | XML

formatToUrl :: ResponseFormat -> String
formatToUrl CSV     = "csv"
formatToUrl GeoJSON = "geojson"
formatToUrl JSON    = "json"
formatToUrl RDFXML  = "rdf"
formatToUrl XML     = "xml"

instance MonadHttp IO where
  handleHttpException = throwIO

-- Misnamed right now because the output is an LbsResponse
--getStringResponse :: Domain -> DatasetID -> ResponseFormat -> RawParameters -> IO String
getStringResponse domain datasetID format query = do
    let url = urlBuilder domain datasetID format
    let param = foldr1 (<>) $ map (\(x,y) -> (pack x) =: y) query
    response <- req GET url NoReqBody lbsResponse param
    return response

getStringBody :: Domain -> DatasetID -> ResponseFormat -> RawParameters -> IO String
getStringBody domain datasetID format query = (getStringResponse domain datasetID format query) >>= (return . L8.unpack . responseBody)

--urlBuilder :: Domain -> DatasetID -> ResponseFormat -> Url Https
urlBuilder domain datasetID format = https domain' /: "resource" /: (datasetID' `append` "." `append` format')
    where domain' = pack domain
          datasetID' = pack datasetID
          format' = pack (formatToUrl format)
