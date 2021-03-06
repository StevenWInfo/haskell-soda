{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
    Module      : Datatypes
    Description : SODA Datatypes
    Copyright   : (c) Steven W
    License     : MIT
    Maintainer  : Steven W <StevenW.Info@gmail.com>
    Stability   : Unstable

These are Haskell types which represent SoQL query types as described on the <https://dev.socrata.com/docs/datatypes Datatypes page> in the SODA documentation.

Geographic values displayed plainly (like in a simple filter or where clause comparison) is displayed in <https://en.wikipedia.org/wiki/Well-known_text Well-known Text (WKT)>.
 -}

module Datatypes
    ( UrlParam
    , SodaError (BadLower)
    , SodaExpr (toUrlParam, lower)
    , Expr (Expr)
    , getVal
    , exprUrlParam
    , Column (Column)
    , SodaVal (SodaVal)
    , SodaType
    -- * Extra typeclasses for constraints
    , SodaNumeric
    , SodaPseudoNumeric
    , SodaOrd
    , SodaGeo
    , SodaSimplifyGeo
    -- * Haskell types corresponding to SODA Types
    , Checkbox
    , Money (..)
    , SodaNum (..)
    , SodaText
    , Timestamp
    , Point (..)
    , MultiPoint
    , Location
    , Line (..)
    , MultiLine
    , Polygon (..)
    , MultiPolygon
    ) where

import Data.List
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format

-- Improve
-- |Indicates what has been interpreted to be put into a URL. The name could possibly use some improvement.
type UrlParam = String

-- Replace Misc with actual things later
data SodaError = BadLower | Misc deriving (Eq, Show)

-- Maybe make an exportable super or sub typeclass so they can use toUrlParam but can't create any instances of type.
-- |The class of all things that can represent a SODA query level type. These include concrete values, columns, and SODA functions.
--class Eq m => SodaExpr m where
class SodaExpr m where
    toUrlParam :: m a -> UrlParam
    lower :: (SodaType a) => m a -> Either SodaError a

-- This makes it even more verbose, but I'm trying to just make it work initially.
-- |Some types require their input to have the type constructor be anonymized. This existential type does just that.
data Expr expr where
    Expr :: (SodaExpr m, SodaType a) => m a -> Expr a
--deriving instance Eq a => Eq (Expr a)
-- instance SodaType a => Eq (Expr a) where
    -- (==) (Expr a) (Expr b) = a == b

instance SodaType a => Show (Expr a) where
    show (Expr a) = toUrlParam a

getVal :: (SodaType a) => Expr a -> Either SodaError a
getVal (Expr a) = lower a

exprUrlParam :: (SodaType a) => Expr a -> UrlParam
exprUrlParam (Expr (expr)) = toUrlParam expr

-- |The type representing a column. The value it holds is just a string of the column's name, but this GADT also carries around information about the type of the column at the query level.
data Column sodatype where
    Column :: (SodaType sodatype) => String -> Column sodatype

instance Eq (Column a) where
    (==) (Column nameA) (Column nameB) = nameA == nameB

instance SodaExpr Column where
    toUrlParam (Column name) = name
    lower col = Left BadLower

-- |A class of all of the Haskell types which correspond with types that SODA uses.
class (Eq sodatype) => SodaType sodatype where
    toUrlPart :: sodatype -> UrlParam

-- |This type just allows us to be able to have Haskell values with Haskell types that correspond with SODA types be able to interact with other SODA expressions like columns and SODA functions.
data SodaVal datatype where
    SodaVal :: (SodaType a) => a -> SodaVal a

instance Eq (SodaVal a) where
    (==) (SodaVal a) (SodaVal b) = a == b

instance SodaExpr SodaVal where
    toUrlParam (SodaVal val) = toUrlPart val
    lower (SodaVal a) = Right a

-- Possibly differentiate this and SodaNum more.
-- |Not to be confused with SodaNum, this is a class with Doubles, Number, and Money in it because these three types can interact.
class (SodaType a) => SodaNumeric a

-- |I'll admit, I named this one this way for the wordplay. This class has Doubles, Number, Money, Timestamp, and SodaText. It's used in Min, Max, and Sum.
class (SodaType a) => SodaPseudoNumeric a

class (SodaType a) => SodaOrd a

-- |For the different geometric SodaTypes.
class (SodaType a) => SodaGeo a

-- |Just for the functions simplify and simplify_preserve_topology.
class (SodaType a) => SodaSimplifyGeo a

-- |The type that corresponds with <https://dev.socrata.com/docs/datatypes/checkbox.html SODA's Checkbox type>. It is the basic ternary type with Nothing as null.
type Checkbox = Maybe Bool
instance SodaType (Maybe Bool) where
    toUrlPart Nothing = "null"
    toUrlPart (Just True) = "true"
    toUrlPart (Just False) = "false"

-- |The type that corresponds with <https://dev.socrata.com/docs/datatypes/money.html SODA's Money type>. The precision beyond the hundreths place doesn't make sense for how most currecies, including the U.S. dollar, is represented, but this datatype can represent any currency whose representation's precision is not necessarily restricted to the hundreths place.
newtype Money = Money { getMoney :: Double } deriving (Show, Eq)
instance SodaType Money where
    toUrlPart m = "'" ++ (show . getMoney $ m) ++ "'"

instance SodaNumeric Money
instance SodaPseudoNumeric Money
instance SodaOrd Money

-- |The type that corresponds with <https://dev.socrata.com/docs/datatypes/double.html SODA's Double type>, is just a Haskell Double type.
instance SodaType Double where
    toUrlPart = show

instance SodaNumeric Double
instance SodaPseudoNumeric Double
instance SodaOrd Double

-- |The type that corresponds with <https://dev.socrata.com/docs/datatypes/double.html SODA's Number type>. Number is actually supposed to have arbitrary precision, and is a bit repetative as a double since we already have double, but I wasn't exactly sure how to implement it. We'll have to look around for true arbitrary precision Haskell types.
newtype SodaNum = SodaNum { getSodaNum :: Double } deriving (Show, Eq)
instance SodaType SodaNum where
    toUrlPart n = (show $ getSodaNum n)

instance SodaNumeric SodaNum
instance SodaPseudoNumeric SodaNum
instance SodaOrd SodaNum

-- |The type that corresponds with <https://dev.socrata.com/docs/datatypes/text.html SODA's Text type>. The difference in the name of the Haskell type and the SODA type is to prevent collisions and confusion with the more popular Haskell Text type.
type SodaText = String
instance SodaType SodaText where
    toUrlPart t = 
        let aposFinder '\'' accum = '\'' : '\'' : accum
            aposFinder char accum = char : accum
        in "'" ++ foldr aposFinder "" t ++ "'"

instance SodaOrd SodaText

-- Cuts off instead of rounding because I have no idea of a good way to handle rounding things like 999.9 milliseconds. If anyone has any better idea of how to handle this, let me know. I suppose I could test and see if the API will handle greater precision, even if it doesn't use it.
-- |The type that corresponds with <https://dev.socrata.com/docs/datatypes/floating_timestamp.html SODA's Floating Timestamp Type>. The name is a bit different because floating timestamp seemed a bit long. The precision and rounding of this type need improvement.
type Timestamp = UTCTime
instance SodaType Timestamp where
    toUrlPart t = (formatTime defaultTimeLocale tsFormat t) ++ "." ++ ms
        where tsFormat = iso8601DateFormat (Just "%T")
              ms = take 3 $ formatTime defaultTimeLocale "%q" t

instance SodaPseudoNumeric Timestamp
instance SodaOrd Timestamp

-- I'm actually not completely sure of the precision required here.
-- Perhaps rename to Position and then have point as a type synonym (since I think that is semantically more correct).
-- Might also want to shorten fields to long and lat or something.
-- |The type that corresponds with <https://dev.socrata.com/docs/datatypes/point.html SODA's Point Type>. I didn't make it a simple tuple because the order of the longitude and latitude differ a bit in places. Also, this is a bit more descriptive.
data Point = Point { longitude :: Double
                   , latitude  :: Double
                   } deriving (Show, Eq)

instance SodaType Point where
    toUrlPart point = "'POINT (" ++ (pointUPart point) ++ ")'"

instance SodaGeo Point

-- This has an alternate WKT format. Have to test it out.
-- |The type that Corresponds with <https://dev.socrata.com/docs/datatypes/multipoint.html SODA's Multipoint type>.
type MultiPoint = [Point]
instance SodaType MultiPoint where
    toUrlPart points = "'MULTIPOINT " ++ (pointsUPart points) ++ "'"

-- Possibly restrict the values used for these.
-- Since the constructor for location is currently not exported, there's currently no reason to export this.
-- |Used as part of the Location type.
data USAddress = USAddress { address :: String
                           , city    :: String
                           , state   :: String
                           , zipCode :: String
                           } deriving (Show, Eq)

-- Use record syntax?
-- One of the developers said on stack overflow that there is no representation for location types in things like a simple filter.
-- Since we're not exporting the constructor and it isn't ever used, then the type definition doesn't really matter.
-- |Corresponds with <https://dev.socrata.com/docs/datatypes/location.html SODA's Location type>. According to the SODA documentation, location is a legacy datatype so it is discouraged from being used and some SODA functions available for the point datatype are not available for the location datatype. The constructor is not exported because there the library currently doesn't know how to represent them in the URL
data Location = Location (Maybe Point) (Maybe USAddress) deriving (Show, Eq)

instance SodaType Location where
    toUrlPart _ = ""

instance SodaGeo Location
                   
-- The only difference in the data structure of Line and Multipoint is that Line has to have at least two positions/points in them
-- |Corresponds with <https://dev.socrata.com/docs/datatypes/line.html SODA's Line type>.
newtype Line = Line { getLinePoints :: [Point] } deriving (Show, Eq)
instance SodaType Line where
    toUrlPart (Line points) = "'LINESTRING " ++ (pointsUPart points) ++ "'"

instance SodaGeo Line
instance SodaSimplifyGeo Line

-- |Corresponds with <https://dev.socrata.com/docs/datatypes/multiline.html SODA's Multiline type>.
type MultiLine = [Line]
instance SodaType MultiLine where
    toUrlPart lines = "'MULTILINESTRING " ++ (linesUPart $ map getLinePoints lines) ++ "'"

instance SodaGeo MultiLine
instance SodaSimplifyGeo MultiLine

-- |Corresponds with <https://dev.socrata.com/docs/datatypes/polygon.html SODA's Polygon type>.
newtype Polygon = Polygon { getPolyPoints :: [[Point]] } deriving (Show, Eq)
instance SodaType Polygon where
    toUrlPart (Polygon lines) = "'POLYGON " ++ (linesUPart lines) ++ "'"

instance SodaGeo Polygon
instance SodaSimplifyGeo Polygon

-- |Corresponds with <https://dev.socrata.com/docs/datatypes/multipolygon.html SODA's Multipolygon type>.
type MultiPolygon = [Polygon]
instance SodaType MultiPolygon where
    toUrlPart polygons = "'MULTIPOLYGON (" ++ (intercalate ", " $ map (linesUPart . getPolyPoints) polygons) ++ ")'"

instance SodaGeo MultiPolygon
instance SodaSimplifyGeo MultiPolygon

-- TODO Bad name. Improve.
-- |Utility function
pointUPart :: Point -> UrlParam
pointUPart (Point long lat) = (show long) ++ " " ++ (show lat)

-- TODO Similarly bad name
-- |Utility function
pointsUPart :: [Point] -> UrlParam
pointsUPart points = "(" ++ (intercalate ", " $ map pointUPart points) ++ ")"

-- |Utility function
linesUPart :: [[Point]] -> UrlParam
linesUPart lines = "(" ++ (intercalate ", " $ map pointsUPart lines) ++ ")"
