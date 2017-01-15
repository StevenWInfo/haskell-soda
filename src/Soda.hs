{-# LANGUAGE OverloadedStrings #-}

{-|
    Module      : Soda
    Description : Sending and Receiving queries.
    Copyright   : (c) Steven W
    License     : MIT
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
    , getLbsResponse
    , getStringBody
    , getSodaResponse
    , Field
    , Row
    , Response
    ) where

import System.IO
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as BS8
import Data.List (foldl')
import Data.Text (Text, pack, append)
import Data.Monoid ((<>), Monoid)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Control.Exception
import Control.Monad (foldM)
import Network.HTTP.Req

import Query
import Datatypes

-- Not sure if I should even export this.
-- |The type that specifies what the parameters are in the request URL. I've made it public so that you can make any call to a SODA, but it isn't recommended. Skipping over using the Query type gets rid of a lot of the compile time guarantees provided by this library.
type RawParameters = [(String, String)]

-- |Specifies what the domain of a request URL should be.
type Domain        = String

-- Maybe just call it Dataset?
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

-- |Required for the req library to throw exceptions.
instance MonadHttp IO where
  handleHttpException = throwIO

-- Todo: put a type declaration on this.
-- Probably don't need to use do notation.
-- getLbsResponse :: Domain -> DatasetID -> ResponseFormat -> RawParameters -> IO String
-- |Gets the whole response. Misnamed right now because it is actually lazy byte strings, and it's returning a response data structure.
getLbsResponse domain datasetID format query = do
    let url = urlBuilder domain datasetID format
    let param = foldr1 (<>) $ map (\(x,y) -> (pack x) =: y) query
    response <- req GET url NoReqBody lbsResponse param
    return response

-- (should probably take just the Query type)
-- |Gets the body of a response from a query given the Domain, DatasetID, ResponseFormat, and query parameters as a list of tuples.
getStringBody :: Domain -> DatasetID -> ResponseFormat -> RawParameters -> IO String
getStringBody domain datasetID format query = (getLbsResponse domain datasetID format query) >>= (return . L8.unpack . responseBody)

-- Possibly shouldn't export this, although I suppose exposing low level stuff could be useful if a user can't do something with the main functions.
--urlBuilder :: Domain -> DatasetID -> ResponseFormat -> Url Https
-- |Builds the non-parameter part of the URL out of the Domain, DatasetID, and ResponseFormat.
urlBuilder domain datasetID format = https domain' /: "resource" /: (datasetID' `append` "." `append` format')
    where domain' = pack domain
          datasetID' = pack datasetID
          format' = pack (formatToUrl format)

-- Change name.
-- |The type to allow you to determine what type the field is being returned as.
data ReturnValue = RCheckbox Checkbox
                | RMoney Money
                | RDouble Double
                | RSodaNum SodaNum
                | RSodaText SodaText
                | RTimestamp Timestamp
                | RPoint Point
                | RMultiPoint MultiPoint
                | RLocation Location
                | RLine Line
                | RMultiLine MultiLine
                | RPolygon Polygon
                | RMultiPolygon MultiPolygon
                deriving (Show, Eq)

-- |The type of the returned field information. The string on the left is the key and the ReturnValue on the right is the value for that field.
type Field = (String, ReturnValue)

-- |The type of a row of returned data.
type Row = [Field]

-- |The type of the response which is all of the rows.
type Response = [Row]

-- |The main way to query information from the Socrata Open Data API. Give it a domain, datasetID, and Query, and it will give you a response interpreted in the established Haskell versions of SODA datatypes.
getSodaResponse :: Domain -> DatasetID -> Query -> IO Response
getSodaResponse domain datasetID query = do
    response <- getLbsResponse domain datasetID JSON (queryToParam query)
    let body = responseBody response
    let responseFields = getResponseFields response
    let responseTypes = getResponseTypes response
    let fieldInfo = zip responseFields responseTypes
    return $ parseResponse fieldInfo body

-- Possibly throw an exception instead of empty?
-- getResponseTypes :: ? -> [String]
-- |Gets the types of the fields which SODA conveniently includes in the header of its response.
getResponseTypes response = case (responseHeader response "X-Soda2-Types") of
    Just header -> (read (BS8.unpack (header))) :: [String]
    Nothing -> []
    
-- getResponseFields :: ? -> [String]
-- |Gets the names of the fields included in the response.
getResponseFields response = case (responseHeader response "X-Soda2-Fields") of
    Just header -> (read (BS8.unpack (header))) :: [String]
    Nothing -> []

--- Parsing stuff. Possibly could use better names. Also need to handle errors better than just default values.

-- |Given the information about all of the fields and the ByteString body, returns a response.
parseResponse :: [(String, String)] -> L8.ByteString -> Response
parseResponse fieldInfo body = case parseMaybe mainParser =<< decode body of
    Just resp -> resp
    Nothing -> []
    where parseArray fInfo arr = mapM (parseRows fInfo) (V.toList arr)
          mainParser = withArray "Array of dataset rows" (parseArray fieldInfo)

-- |Given the info for all fields, and the aeson object value for a row, parse the row into Haskell values.
parseRows :: [(String, String)] -> Value -> Parser Row
parseRows fieldInfo rowObj = withObject "Row" objToParser rowObj
    where objToParser o = foldM (parseField rowObj) [] fieldInfo
    
-- |Folding function which, given the aeson object value for a row, a potentially partially parsed row, and the metadata information for a field, returns the parsed row with the newly parsed field added to it.
parseField :: Value -> Row -> (String, String) -> Parser Row
parseField (Object obj) accum ((key, fieldType)) = case HM.lookup (pack key) obj of
    Nothing  -> return accum
    Just val -> (:) <$> (fmap ((,) key) (parseReturnVal fieldType val)) <*> (pure accum)
                   

-- There's probably a simpler and terser way to do this.
-- |Uses the type information included in the header of the response to tell parseJSON what types to parse the JSON into.
parseReturnVal :: String -> Value -> Parser ReturnValue
parseReturnVal fieldType val = case fieldType of
    "checkbox"     -> fmap RCheckbox ((parseJSON val) :: Parser Checkbox)
    "money"        -> fmap RMoney ((parseJSON val) :: Parser Money)
    "double"       -> fail "Doubles don't work yet because of the format that SODA returns them in." -- fmap RDouble ((parseJSON val) :: Parser Double)
    "number"       -> fmap RSodaNum ((parseJSON val) :: Parser SodaNum)
    "text"         -> fmap RSodaText ((parseJSON val) :: Parser SodaText)
    "timestamp"    -> fmap RTimestamp ((parseJSON val) :: Parser Timestamp)
    "point"        -> fmap RPoint ((parseJSON val) :: Parser Point)
    "multipoint"   -> fmap RMultiPoint ((parseJSON val) :: Parser MultiPoint)
    "location"     -> fail "Not sure how to parse location types" -- fmap RLocation ((parseJSON val) :: Parser Location)
    "line"         -> fmap RLine ((parseJSON val) :: Parser Line)
    "multiline"    -> fmap RMultiLine ((parseJSON val) :: Parser MultiLine)
    "polygon"      -> fmap RPolygon ((parseJSON val) :: Parser Polygon)
    "multipolygon" -> fmap RMultiPolygon ((parseJSON val) :: Parser MultiPolygon)
