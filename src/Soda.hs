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
    , getLbsResponse
    , getStringBody
    ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as BS8
import System.IO
import Data.List (foldl')
import Data.Text (Text, pack, append)
import Data.Monoid ((<>), Monoid)
--import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Network.HTTP.Req
import Control.Exception
import Control.Monad (foldM)

import Query
import Datatypes

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

-- For the function which interprets directly into Haskell values, we don't need the response format since it won't affect what they get. We can always just use JSON or something.

-- Misnamed right now because the output is an LbsResponse
-- Also, probably don't need to use do notation.
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

-- Maybe this should also take the parameters.
--urlBuilder :: Domain -> DatasetID -> ResponseFormat -> Url Https
-- |Builds the non-parameter part of the URL out of the Domain, DatasetID, and ResponseFormat.
urlBuilder domain datasetID format = https domain' /: "resource" /: (datasetID' `append` "." `append` format')
    where domain' = pack domain
          datasetID' = pack datasetID
          format' = pack (formatToUrl format)

-- Create a string URL builder.

-- Change name.
data ReturnType = RCheckbox Checkbox
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

type Row = [(String, ReturnType)]

type Response = [Row]

-- IO Response?
-- TODO try and catch the responce and put into either. Could also use req's settings to change automatically into either.
-- Return an IO Either response (or probably EitherT IO response)
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
getResponseTypes response = case (responseHeader response "X-Soda2-Types") of
    Just header -> (read (BS8.unpack (header))) :: [String]
    Nothing -> []
    
-- getResponseFields :: ? -> [String]
getResponseFields response = case (responseHeader response "X-Soda2-Fields") of
    Just header -> (read (BS8.unpack (header))) :: [String]
    Nothing -> []

--- Parsing stuff. Need better names. Also need to handle errors better than just default values.

-- Need to consider responses that don't have fields for certain rows. Maybe make all fields be maybe.
parseResponse :: [(String, String)] -> L8.ByteString -> Response
parseResponse fieldInfo body = case parseMaybe mainParser =<< decode body of
    Just resp -> resp
    Nothing -> []
    where parseArray fInfo arr = mapM (parseRows fInfo) (V.toList arr)
          mainParser = withArray "Array of dataset rows" (parseArray fieldInfo)

parseRows :: [(String, String)] -> Value -> Parser Row
parseRows fieldInfo rowObj = withObject "Row" objToParser rowObj
    where objToParser o = foldM (parseField rowObj) [] fieldInfo
    
-- folded function
parseField :: Value -> Row -> (String, String) -> Parser Row
parseField (Object obj) accum ((key, fieldType)) = case HM.lookup (pack key) obj of
    Nothing  -> accum
    Just val -> (fmap ((,) key) (parseReturnVal fieldType val)) : accum
    {-
    Just val -> case parseMaybe (parseReturnVal fieldType) =<< val of
                     Nothing -> accum
                     Just returnVal -> (key, returnVal) : accum
                     -}
                   

-- There's probably a simpler and terser way to do this.
parseReturnVal :: String -> Value -> Parser ReturnType
parseReturnVal fieldType val = case fieldType of
    "checkbox"     -> fmap RCheckbox ((parseJSON val) :: Parser Checkbox)
    "money"        -> fmap RMoney ((parseJSON val) :: Parser Money)
    "double"       -> fmap RDouble ((parseJSON val) :: Parser Double)
    "number"       -> fmap RSodaNum ((parseJSON val) :: Parser SodaNum)
    "text"         -> fmap RSodaText ((parseJSON val) :: Parser SodaText)
    "timestamp"    -> fmap RTimestamp ((parseJSON val) :: Parser Timestamp)
    "point"        -> fmap RPoint ((parseJSON val) :: Parser Point)
    "multipoint"   -> fmap RMultiPoint ((parseJSON val) :: Parser MultiPoint)
    "location"     -> fmap RLocation ((parseJSON val) :: Parser Location)
    "line"         -> fmap RLine ((parseJSON val) :: Parser Line)
    "multiline"    -> fmap RMultiLine ((parseJSON val) :: Parser MultiLine)
    "polygon"      -> fmap RPolygon ((parseJSON val) :: Parser Polygon)
    "multipolygon" -> fmap RMultiPolygon ((parseJSON val) :: Parser MultiPolygon)
