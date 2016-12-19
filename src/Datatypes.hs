{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Datatypes
    ( SodaExpr
    , SodaTypes
    , SodaTypeBox
    , Column
    ) where

import Data.List
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format

{-|
SODA datatypes

Geographic values displayed plainly (like in a simple filter or where clause comparison) is displayed in Well-known Text (WKT).
 -}

 {-
  - Have to make sure that the URL parameter serializations use the correct characters when serialization (like $ isn't confused with a parameter).
  -
  - I could make some of my own functions that use the SoQL functions as well.
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
    Column :: (SodaTypes sodatype) => String -> Column sodatype

instance SodaExpr Column where
    toUrlParam (Column name) = name

-- Numbers, Doubles, and Moneys are numeric types that can interact. Might need to make an instance of numeric or a custom typeclass if I don't want them interacting with other types.

-- Currently flawed in a few ways. Might be good to get a basic version working though and improving from there.
-- Will need to test these type constraints further.
-- Even if I have to refactor/redesign this, documenting their types is useful.
data SodaVal datatype where
    SodaVal :: SodaTypes a => a -> SodaVal a

instance SodaExpr SodaVal where
    toUrlParam (SodaVal val) = toUrlPart val

-- Maybe make more descriptive types later.
-- Would some more dependent type features have made this simpler?
data SodaFunc datatype where
    Avg :: SodaTypes a => Column a -> SodaFunc Number -- Aggregate
    Between :: (SodaExpr m, SodaExpr sodaExpr, SodaExpr sodaExprAlt, SodaType a, SodaTypes sodaType) => m a -> sodaExpr sodaType -> sodaExprAlt sodaType -> SodaFunc Checkbox -- Need another type constraint on sodaType
    Case :: (SodaExpr m, SodaExpr n, SodaTypes a) => [(m Checkbox, n a)] -> SodaFunc a -- Can the condition have a checkbox value/
    ConvexHull :: (SodaTypes geo) => Column geo -> SodaFunc MultiPolygon -- Geo typeclass. I think that it has to be a column, but I'm not sure.
    Count :: SodaTypes a => Column a -> SodaFunc Number
    DateTruncY :: (SodaExpr m) => m Timestamp -> SodaFunc Timestamp
    DateTruncYM :: (SodaExpr m) => m Timestamp -> SodaFunc Timestamp
    DateTruncYMD :: (SodaExpr m) => m Timestamp -> SodaFunc Timestamp
    Distance :: (SodaExpr m, SodaExpr n) => m Point -> n Point -> SodaFunc Number
    Extent :: (SodaExpr m, SodaTypes geo) => m geo -> SodaFunc MultiPolygon -- Takes an agg (can't use in where)
    In :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> [n b] -> SodaFunc Checkbox -- Input needs to be constrained
    Intersects :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> n b -> SodaFunc Checkbox
    Like :: (SodaExpr m, SodaExpr n) => m SodaText -> n SodaText -> SodaFunc Checkbox
    Lower :: (SodaExpr m) => m SodaText -> SodaFunc SodaText
    Max :: (SodaTypes a) => Column a -> SodaFunc a -- Special constraints
    Min :: (SodaTypes a) => Column a -> SodaFunc a -- Special constraints
    NotBetween :: (SodaExpr m, SodaExpr n, SodaTypes a) => m a -> n a -> SodaFunc Checkbox
    NotIn :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> n b -> SodaFunc Checkbox -- Input needs to be constrained
    NotLike :: (SodaExpr m, SodaExpr n) => m SodaText -> n SodaText -> SodaFunc Checkbox
    NumPoints :: (SodaExpr m, SodaTypes geo) => m geo -> SodaFunc Number -- Geo constraint
    Simplify :: (SodaExpr m, SodaExpr n, SodaTypes geoAlt) => m geoAlt -> n Number -> SodaFunc geoAlt -- Alternative geo constraint. Need to test this.
    SimplifyPreserveTopology :: (SodaExpr m, SodaExpr n, SodaTypes geoAlt) => m geoAlt -> n Number -> SodaFunc geoAlt -- Alternative geo constraint. Need to test this. Better name?
    StartsWith :: (SodaExpr m, SodaExpr n) => m SodaText -> n SodaText -> SodaFunc Checkbox
    StdDevPop :: (SodaTypes num) => Column num -> SodaFunc Number -- Num constraint. First parameter might be Agg instead of Column
    StdDevSamp :: (SodaTypes num) => Column num -> SodaFunc Number -- Num constraint. First parameter might be Agg instead of Column
    Sum :: Column Number -> SodaFunc Number
    Upper :: (SodaExpr m) => m SodaText -> SodaFunc SodaText
    WithinBox :: (SodaExpr m, SodaExpr n, SodaExpr o, SodaExpr p, SodaExpr q, SodaTypes geo) => m geo -> n Point -> o Point -> p Point -> q Point -> SodaFunc Checkbox -- Geo constraint that includes location
    WithinCircle :: (SodaExpr m, SodaExpr n, SodaExpr o, SodaExpr p, SodaTypes geo) => m geo -> n Point -> o Point -> p Number -> SodaFunc Checkbox -- Geo constraint that includes location
    WithinPolygon :: (SodaExpr m, SodaExpr n, SodaTypes geo) => m geo -> n MultiPolygon -> SodaFunc Checkbox -- Geo constraint that doesn't include location.
    {-

    Other things like operators?
    =
    <
    >
    -}

{- I don't think any automatic scoping with parenthesis is necessary, but I might need it. Might be good to have a manual one just in case, for people who want it, and for other parts that need it.
scoper :: (SodaExpr m) => m a -> UParam
scoper SodaFunc a = "(" ++ (toUrlParam (SodaFunc a) ++ ")"
scoper = toUrlParam
-}

-- Should the capitalization be consistant?
instance SodaExpr SodaFunc where
    toUrlParam (Avg col) = "avg(" ++ (toUrlParam col) ++ ")"
    toUrlParam (Between val first last) = (toUrlParam val) ++ " between " ++ (toUrlParam first) ++ " and " ++ (toUrlParam last)
    toUrlParam (Case paths) = "case(" ++ (commaSeperated (map (\(cond, result) -> (toUrlParam cond) ++ ", " ++ (toUrlParam result)))) ++ ")"
    toUrlParam (ConvexHull shape) = "convex_hull(" ++ toUrlParam ++ ")"
    toUrlParam (Count rows) = "count(" ++ (toUrlParam rows) ++ ")"
    toUrlParam (DateTruncY time) = "date_trunc_y(" ++ (toUrlParam time) ++ ")"
    toUrlParam (DateTruncYM time) = "date_trunc_ym(" ++ (toUrlParam) ++ ")"
    toUrlParam (DateTruncYMD time) = "date_trunc_ymd(" ++ (toUrlParam) ++ ")"
    toUrlParam (Distance pointA pointB) = "distance_in_meters(" ++ (toUrlParam pointA) ++ ", " ++ (toUrlParam pointB) ++ ")"
    toUrlParam (Extent points) = "extent(" ++ (toUrlParam points) ++ ")"
    toUrlParam (In element values) = (toUrlParam element) ++ " IN(" ++ commaSeperated (map toUrlParam values) ++ ")"
    toUrlParam (Intersects shapeA shapeB) = "intersects(" ++ (toUrlParam shapeA) ++ ", " (toUrlParam shapeB) ++ ")"
    toUrlParam (Like textA textB) = (toUrlParam textA) ++ " like " ++ (toUrlParam textB)
    toUrlParam (Lower text) = "lower(" ++ (toUrlParam text) ++ ")"
    toUrlParam (Max numbers) = "max(" ++ (toUrlParam numbers) ++ ")"
    toUrlParam (Min numbers) = "min(" ++ (toUrlParam numbers) ++ ")"
    toUrlParam (NotBetween val first last) = (toUrlParam val) ++ " not between " ++ (toUrlParam first) ++ " and " ++ (toUrlParam last)
    toUrlParam (NotIn element values) = (toUrlParam element) ++ " not in(" ++ commaSeperated (map toUrlParam values) ++ ")"
    toUrlParam (NotLike textA textB) =  (toUrlParam textA) ++ " not like " ++ (toUrlParam textB)
    toUrlParam (NumPoints points) = "num_points(" ++ (toUrlParam points) ++ ")"
    toUrlParam (Simplify geometry tolerance) = "simplify(" ++ (toUrlParam geometry) ++ ", " ++ (toUrlParam tolerance) ++ ")"
    toUrlParam (SimplifyPreserveTopology geometry tolerance) = "simplify_preserve_topology(" ++ (toUrlParam geometry) ++ ", " ++ (toUrlParam tolerance) ++ ")"
    toUrlParam (StartsWith haystack needle) = "starts_with(" ++ (toUrlParam haystack) ++ ", " ++ (toUrlParam needle) ++ ")"
    toUrlParam (StdDevPop 
    toUrlParam (StdDevSamp 
    toUrlParam (Sum 
    toUrlParam (Upper 
    toUrlParam (WithinBox 
    toUrlParam (WithinCircle 
    toUrlParam (WithinPolygon 

-- This is going to take a while.
--instance SodaExpr (SodaFunc sodatype) where

-- Some of these might have conflicting names.
-- Sort of want an implicit bool type as well, but it's a bit tricky to implement
-- |Corresponds to ternary valued values with Nothing as null.
type Checkbox = Maybe Bool
instance SodaTypes (Maybe Bool) where
    toUrlPart Nothing = "null"
    toUrlPart (Just True) = "true"
    toUrlPart (Just False) = "false"

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
    toUrlPart t = (formatTime defaultTimeLocale tsFormat t) ++ ms
        where tsFormat = iso8601DateFormat (Just "%T")
              ms = take 3 $ formatTime defaultTimeLocale "%q" t

-- TODO Bad name. Improve.
-- |Utility function
pointUPart :: Point -> UParam
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
commaSeperated :: [UParam] -> UParam
commaSeperated [] = ""
commaSeperated (first:rest) = foldl' combiner first rest
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
    MkSodaType :: SodaTypes a => a -> SodaTypeBox
