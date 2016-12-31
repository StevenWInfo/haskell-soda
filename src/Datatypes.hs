{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Datatypes
    ( SodaTypes
    , SodaExpr (toUrlParam)
    , Column (Column)
    , SodaVal (SodaVal)
    , Checkbox
    , Money (..)
    , Number (..)
    , SodaText
    , Timestamp
    , Point (..)
    , MultiPoint
    , Location (..)
    , USAddress (..)
    , Line (..)
    , MultiLine
    , Polygon (..)
    , MultiPolygon
    , UrlParam
    ) where

import Data.List
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format

{-|
SODA datatypes

Geographic values displayed plainly (like in a simple filter or where clause comparison) is displayed in Well-known Text (WKT).
 -}

-- Improve
type UrlParam = String

-- Maybe make an exportable super or sub typeclass so they can use toUrlParam but can't create any instances of type.
class SodaExpr m where
    toUrlParam :: m a -> UrlParam

class SodaTypes sodatype where
    toUrlPart :: sodatype -> UrlParam

data Column sodatype where
    Column :: (SodaTypes sodatype) => String -> Column sodatype

instance SodaExpr Column where
    toUrlParam (Column name) = name

data SodaVal datatype where
    SodaVal :: SodaTypes a => a -> SodaVal a

instance SodaExpr SodaVal where
    toUrlParam (SodaVal val) = toUrlPart val

-- Perhaps a true ternary type would be better. I'm not sure.
-- |Corresponds to ternary valued values with Nothing as null.
-- data Checkbox = SodaTrue | SodaFalse | SodaNull -- Would make typing out stuff more consistant.
type Checkbox = Maybe Bool
instance SodaTypes (Maybe Bool) where
    toUrlPart Nothing = "null"
    toUrlPart (Just True) = "true"
    toUrlPart (Just False) = "false"

newtype Money = Money { getMoney :: Double } deriving (Show)
instance SodaTypes Money where
    toUrlPart m = show m

instance SodaTypes Double where
    toUrlPart d = show d

newtype Number = Number { getNumber :: Double } deriving (Show)
instance SodaTypes Number where
    toUrlPart n = show $ getNumber n

-- |The name inconsistancy is to prevent inconsistancies with the popular Text type.
type SodaText = String
instance SodaTypes SodaText where
    toUrlPart t = "'" ++ t ++ "'"

-- Cuts off instead of rounding because I have no idea of a good way to handle rounding things like 999.9 milliseconds. If anyone has any better idea of how to handle this, let me know. I suppose I could test and see if the API will handle greater precision, even if it doesn't use it.
type Timestamp = UTCTime
instance SodaTypes Timestamp where
    toUrlPart t = (formatTime defaultTimeLocale tsFormat t) ++ ms
        where tsFormat = iso8601DateFormat (Just "%T")
              ms = take 3 $ formatTime defaultTimeLocale "%q" t

-- TODO Bad name. Improve.
-- |Utility function
pointUPart :: Point -> UrlParam
pointUPart (Point long lat) = (show long) ++ " " ++ (show lat)

-- I'm actually not completely sure of the precision required here.
-- Perhaps rename to Position and then have point as a type synonym (since I think that is semantically more correct).
-- The toUrlPart for this one is a bit weird. Expr will have to have it's own serialize function for SoQL functions because points differ in those.
-- Does point not even work in a simple filter?
data Point = Point { longitude :: Double
                   , latitude  :: Double
                   } deriving (Show)
instance SodaTypes Point where
    toUrlPart point = "'POINT (" ++ (pointUPart point) ++ ")'"

-- TODO Similarly bad name
-- |Utility function
pointsUPart :: [Point] -> UrlParam
pointsUPart points = "(" ++ (intercalate "," $ map pointUPart points) ++ ")"
    

-- This has an alternate WKT format. Have to test it out.
type MultiPoint = [Point]
instance SodaTypes MultiPoint where
    toUrlPart points = "'MULTIPOINT " ++ (pointsUPart points) ++ "'"

-- Possibly restrict the values used for these.
data USAddress = USAddress { address :: String
                           , city    :: String
                           , state   :: String
                           , zip     :: String
                           }

-- Use record syntax?
-- One of the developers said on stack overflow that there is no representation for location types in things like a simple filter.
-- |According to the SODA documentation, location is a legacy datatype so it is discouraged from being used and some SODA functions available for the point datatype are not available for the location datatype.
data Location = Location (Maybe Point) (Maybe USAddress)
instance SodaTypes Location where
    toUrlPart _ = ""
                   
-- The only difference in the data structure of Line and Multipoint is that Line has to have at least two positions/points in them
newtype Line = Line { getLinePoints :: [Point] } deriving (Show)
instance SodaTypes Line where
    toUrlPart (Line points) = "'LINESTRING " ++ (pointsUPart points) ++ "'"

linesUPart :: [[Point]] -> UrlParam
linesUPart lines = "(" ++ (intercalate "," $ map pointsUPart lines) ++ ")"

type MultiLine = [Line]
instance SodaTypes MultiLine where
    toUrlPart lines = "'MULTILINESTRING " ++ (linesUPart $ map getLinePoints lines) ++ "'"

newtype Polygon = Polygon { getPolyPoints :: [[Point]] }
instance SodaTypes Polygon where
    toUrlPart (Polygon lines) = "'POLYGON " ++ (linesUPart lines)

type MultiPolygon = [Polygon]
instance SodaTypes MultiPolygon where
    toUrlPart polygons = "'MULTIPOLYGON (" ++ (intercalate "," $ map (linesUPart . getPolyPoints) polygons) ++ ")'"
