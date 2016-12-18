{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Datatypes
    ( Expr
    , SodaTypes
    , SodaTypes
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

-- Improve
type UParam = String

class SodaExpr m where
    toUrlParam :: m a -> UParam

-- I think I actually want the toUrlPart to be a function on Expr
class SodaTypes sodatype where
    toUrlPart :: sodatype -> UParam

-- Perhaps need to restrict this further whereever this is used to exclude things like whitespace. Not sure if should do at value or type level.
data Column sodatype where
    Column :: SodaTypes sodatype => String -> Column sodatype

instance SodaExpr (Column sodatype) where
    toUrlParam (Column name) = name

-- Numbers, Doubles, and Moneys are numeric types that can interact. Might need to make an instance of numeric or a custom typeclass if I don't want them interacting with other types.

-- Currently flawed in a few ways. Might be good to get a basic version working though and improving from there.
-- Will need to test these type constraints further.
-- Even if I have to refactor/redesign this, documenting their types is useful.
data SodaVal datatype where
    SodaVal :: SodaTypes a => a -> SodaVal a

instance SodaExpr (SodaVal sodatype) where
    toUrlParam (SodaVal val) = toUrlPart val

-- Maybe make more descriptive types later.
data SodaFunc datatype where
    Avg :: SodaTypes a => Column a -> SodaFunc Number -- Aggregate
    Between :: (SodaExpr sodaExpr, SodaTypes sodaType) => sodaExpr sodaType -> sodaExprAlt sodaType -> SodaFunc Bool -- Need another type constraint on sodaType
    Case :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m Checkbox -> n b -> SodaFunc b -- Can the condition have a checkbox value/
    ConvexHull :: Column geo -> Expr MultiPolygon -- Geo typeclass. I think that it has to be a column, but I'm not sure.
    Count :: SodaTypes a => Column a -> Expr Number
    DateTruncY :: Expr Timestamp -> Expr Timestamp
    DateTruncYM :: Expr Timestamp -> Expr Timestamp
    DateTruncYMD :: Expr Timestamp -> Expr Timestamp
    Distance :: Expr Point -> Expr Point -> Expr Number
    Extent :: Expr geo -> Expr MultiPolygon -- Takes an agg (can't use in where)
    In :: Expr i -> Expr Bool -- Input needs to be constrained
    Intersects :: Expr geo -> Expr geo -> Expr Bool
    Like :: Expr SodaText -> Expr SodaText -> Expr Bool
    Lower :: Expr SodaText -> Expr SodaText
    Max :: Column a -> Expr a -- Special constraints
    Min :: Column a -> Expr a -- Special constraints
    NotBetween :: (SodaTypes a) => Expr a -> Expr a -> Expr Bool
    NotIn :: Expr i -> Expr Bool -- Input needs to be constrained
    NotLike :: Expr SodaText -> Expr SodaText -> Expr Bool
    NumPoints :: Expr geo -> Expr Number -- Geo constraint
    Simplify :: Expr geoAlt -> Expr Number -> Expr geoAlt -- Alternative geo constraint. Need to test this.
    SimplifyPreserveTopology  :: Expr geoAlt -> Expr Number -> Expr geoAlt -- Alternative geo constraint. Need to test this. Better name?
    StartsWith :: Expr SodaText -> Expr SodaText -> Expr Bool
    StdDevPop :: Column num -> Expr Number -- Num constraint. First parameter might be Agg instead of Column
    StdDevSamp :: Column num -> Expr Number -- Num constraint. First parameter might be Agg instead of Column
    Sum :: Column Number -> Expr Number
    Upper :: Expr SodaText -> Expr SodaText
    WithinBox :: Expr geo -> Expr Point -> Expr Point -> Expr Point -> Expr Point -> Expr Bool -- Geo constraint that includes location
    WithinCircle :: Expr geo -> Expr Point -> Expr Point -> Expr Number -> Expr Bool -- Geo constraint that includes location
    WithinPolygon :: Expr geo -> Expr MultiPolygon -> Expr Bool -- Geo constraint that doesn't include location.
    {-

    Other things like operators?
    =
    <
    >
    -}

-- Some of these might have conflicting names.
-- Sort of want an implicit bool type as well, but it's a bit tricky to implement
-- |Corresponds to ternary valued values with Nothing as null.
type Checkbox = Maybe Bool
instance SodaTypes (Maybe Bool) where
    toUrlPart Nothing = "null"
    toUrlPart Just True = "true"
    toUrlPart Just Fail = "false"

-- Not sure if money is just fixed precision or more complicated. Round until I find a way to use a better type.
-- If we're just talking about US dollars, I suppose I could record as an integer of cents.
newtype Money = Money { getMoney :: Double } deriving (Show)
instance SodaTypes Money where
    toUrlPart m = show m

-- Maybe make these be not empty.
instance SodaTypes Double where
    toUrlPart d = show d

-- Eventually replace with a better type. Look at the numbers package
newtype Number = Number { getNumber :: Double } deriving (Show)
instance SodaTypes Number where
    toUrlPart n = show $ getNumber n

-- TODO Escape single quotes.
-- |The name inconsistancy is to prevent inconsistancies with the popular Text type.
type SodaText = String
instance SodaTypes SodaText where
    toUrlPart t = "'" ++ t ++ "'"

-- Cuts off instead of rounding because I have no idea of a good way to handle rounding things like 999.9 milliseconds. If anyone has any better idea of how to handle this, let me know. I suppose I could test and see if the API will handle greater precision, even if it doesn't use it.
type Timestamp = UTCTime
instance SodaTypes Timestamp where
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
instance SodaTypes Point where
    toUrlPart point = "'POINT (" ++ (pointUPart point) ++ ")'"

-- |Utility function. I saw that there's an even simpler function for this in Data.List
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
instance SodaTypes MultiPoint where
    toUrlPart points = "'MULTIPOINT " ++ (pointsUPart points) ++ "'"

-- Possibly restrict the values used for these.
data USAddress = USAddress { address :: String
                           , city    :: String
                           , state   :: String
                           , zip     :: String
                           }

-- Should really require one of these not to be Nothing, but can put that restriction in the smart constructor.
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

linesUPart :: [[Point]] -> UParam
linesUPart lines = "(" ++ (commaSeperated $ map pointsUPart lines) ++ ")"

type MultiLine = [Line]
instance SodaTypes MultiLine where
    toUrlPart lines = "'MULTILINESTRING " ++ (linesUPart $ map getLinePoints lines) ++ "'"

newtype Polygon = Polygon { getPolyPoints :: [[Point]] }
instance SodaTypes Polygon where
    toUrlPart (Polygon lines) = "'POLYGON " ++ (linesUPart lines)

type MultiPolygon = [Polygon]
instance SodaTypes MultiPolygon where
    toUrlPart polygons = "'MULTIPOLYGON (" ++ (commaSeperated $ map (linesUPart . getPolyPoints) polygons) ++ ")'"

-- A bit confusing to use sodatype as a parameter all over the place, and have SodaTypes as an existential type.
-- Fix this
-- |Existential type representing one of the soda data types.
data SodaTypeBox where
    MkSodaType :: SodaTypeBox a => a -> SodaTypeBox
