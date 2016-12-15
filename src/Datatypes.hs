{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Datatypes
    ( Expr
    , SodaClass
    , SodaType
    ) where

import Data.Time.Calendar
import Data.Time.Clock

{-|
SODA datatypes

Geographic values displayed plainly (like in a simple filter or where clause comparison) is displayed in Well-known Text (WKT).
 -}

 {-
  - Have to make sure that the URL parameter serializations use the correct characters when serialization (like $ isn't confused with a parameter).
  -}

-- Numbers, Doubles, and Moneys are numeric types that can interact. Might need to make an instance of numeric or a custom typeclass if I don't want them interacting with other types.

data Expr datatype where
    SodaVal :: SodaClass a => a -> Expr a
{-
    Checkbox          :: Checkbox -> Expr Checkbox
    MoneyE            :: Money -> Expr Money
    Double            :: Double -> Expr Double
    NumberE           :: Number -> Expr Number
    Text              :: Text -> Expr Text
    FloatingTimestamp :: FloatingTimestamp -> Expr FloatingTimestamp
    PointE            :: Point -> Expr Point
    MultiPointE       :: MultiPoint -> Expr MultiPoint
    LocationE         :: Location -> Expr Location
    LineE             :: Line -> Expr Line
    MultiLineE        :: MultiLine -> Expr MultiLine
    PolygonE          :: Polygon -> Expr Polygon
    MultiPolygonE     :: MultiPolygon -> Expr MultiPolygon
    -}
    --Sum             :: Column (Expr Int)

--data Func = Sum (Column (Expr Int)) -- ?
    -- | StartsWith (Column (Expr

-- Improve
type UParam = String

-- I think I actually want the toUrlPart to be a function on Expr
class SodaClass sodatype where
    toUrlPart :: sodatype -> UParam

-- Some of these might have conflicting names.
-- |Corresponds to ternary valued values with Nothing as null.
type Checkbox = Maybe Bool
instance SodaClass (Maybe Bool) where
    toUrlPart Nothing = "null"
    toUrlPart Just True = "true"
    toUrlPart Just Fail = "false"

-- Not sure if money is just fixed precision or more complicated. Round until I find a way to use a better type.
-- If we're just talking about US dollars, I suppose I could record as an integer of cents.
newtype Money = Money { getMoney :: Double } deriving (Show)
instance SodaClass Money where
    toUrlPart m = show m

-- Maybe make these be not empty.
instance SodaClass Double where
    toUrlPart d = show d

-- Eventually replace with a better type. Look at the numbers package
newtype Number = Number { getNumber :: Double } deriving (Show)
instance SodaClass Number where
    toUrlPart n = show $ getNumber n

-- TODO Escape single quotes.
-- |The name inconsistancy is to prevent inconsistancies with the popular Text type.
type SodaText = String
instance SodaClass SodaText where
    toUrlPart t = "'" ++ t ++ "'"

-- Cuts off instead of rounding because I have no idea of a good way to handle rounding things like 999.9 milliseconds. If anyone has any better idea of how to handle this, let me know. I suppose I could test and see if the API will handle greater precision, even if it doesn't use it.
type FloatingTimestamp = UTCTime
instance SodaClass FloatingTimestamp where
    toUrlPart t = (formatTime defaultTimeLocale tsFormat t) ++ roundedMS
        where tsFormat = iso8601DateFormat (Just "%T")
              ms = take 3 $ formatTime "%q" t

-- TODO Bad name. Improve.
-- |Utility function
pointUrlPart :: Point -> UParam
pointUPart (Point long lat) = (show long) ++ " " ++ (show lat)

-- I'm actually not completely sure of the precision required here.
-- Perhaps rename to Position and then have point as a type synonym (since I think that is semantically more correct).
-- The toUrlPart for this one is a bit weird. Expr will have to have it's own serialize function for SoQL functions because points differ in those.
-- Does point not even work in a simple filter?
data Point = Point { longitude :: Double
                   , latitude  :: Double
                   } deriving (Show)
instance SodaClass Point where
    toUrlPart point = "'POINT (" ++ (pointUPart point) ++ ")'"

-- |Utility function
commaSeperated :: [a] -> UParam
commaSeperated [] = ""
commaSeperated (first:others) = foldl' combiner first rest
    where combiner accum x = accum ++ ", " ++ x

-- TODO Similarly bad name
-- |Utility function
pointsUPart :: [Point] -> UParam
pointsUPart points = "(" ++ (commaSeperated $ map pointUPart points) ++ ")"
    

-- This has an alternate WKT format. Have to test it out.
-- TODO check if 
type MultiPoint = [Point]
instance SodaClass MultiPoint where
    toUrlPart points = "'MULTIPOINT " ++ (pointsUPart points) ++ "'"

-- Possibly restrict the values used for these.
data USAddress = USAddress { address :: String
                           , city    :: String
                           , state   :: String
                           , zip     :: String
                           }

-- Should really require one of these not to be Nothing, but can put that restriction in the smart constructor.
-- Use record syntax?
-- I have no idea how this would be put into the parameter format. Ask on stackoverflow, but it might not be possible.
-- |According to the SODA documentation, location is a legacy datatype so it is discouraged from being used and some SODA functions available for the point datatype are not available for the location datatype.
data Location = Location (Maybe Point) (Maybe USAddress)
instance SodaClass Location where
    toUrlPart _ = ""
                   
-- The only difference in the data structure of Line and Multipoint is that Line has to have at least two positions/points in them
newtype Line = Line { getLinePoints :: [Point] } deriving (Show)
instance SodaClass Line where
    toUrlPart (Line points) = "'LINESTRING " ++ (pointsUPart points) ++ "'"

linesUPart :: [[Point]] -> UParam
linesUPart lines = "(" ++ (commaSeperated $ map pointsUPart lines) ++ ")"

type MultiLine = [Line]
instance SodaClass MultiLine where
    toUrlPart lines = "'MULTILINESTRING " ++ (linesUPart $ map getLinePoints lines) ++ "'"

newtype Polygon = Polygon { getPolyPoints :: [[Point]] }
instance SodaClass Polygon where
    toUrlPart (Polygon lines) = "'POLYGON " ++ (linesUPart lines)

type MultiPolygon = [Polygon]
instance SodaClass MultiPolygon where
    toUrlPart polygons = "'MULTIPOLYGON (" ++ (commaSeperated $ map (linesUPart . getPolyPoints) polygons) ++ ")'"

-- A bit confusing to use sodatype as a parameter all over the place, and have SodaType as an existential type.
-- |Existential type representing one of the soda data types.
data SodaType where
    MkSodaType :: SodaClass a => a -> SodaType

-- SoSQL functions
-- TODO

-- Paging functionality
-- TODO
