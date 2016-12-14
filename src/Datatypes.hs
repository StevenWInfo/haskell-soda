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

class SodaClass sodatype where
    toUrlPart :: sodatype -> String

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

-- I'm actually not completely sure of the precision required here.
-- Perhaps rename to Position and then have point as a type synonym (since I think that is semantically more correct).
-- The toUrlPart for this one is a bit weird. Expr will have to have it's own serialize function for SoQL functions because points differ in those.
-- Does point not even work in a simple filter?
data Point = Point { longitude :: Double
                   , latitude  :: Double
                   } deriving (Show)
instance SodaClass Point where
    toUrlPart (Point long lat) = "(" ++ (show long) ++ ", " ++ (show lat) ++ ")"

type MultiPoint = [Point]
instance SodaClass MultiPoint

-- Possibly restrict the values used for these.
data USAddress = USAddress { address :: String
                           , city    :: String
                           , state   :: String
                           , zip     :: String
                           }

-- Should really require one of these not to be Nothing, but can put that restriction in the smart constructor.
-- Use record syntax?
data Location = Location (Maybe Point) (Maybe USAddress)
instance SodaClass Location
                   
-- The only difference in the data structure of Line and Multipoint is that Line has to have at least two positions/points in them
newtype Line = Line { getLine :: [Point] } deriving (Show)
instance SodaClass Line

type MultiLine = [Line]
instance SodaClass MultiLine

type Polygon = [Point]

type MultiPolygon = [Polygon]

-- A bit confusing to use sodatype as a parameter all over the place, and have SodaType as an existential type.
-- |Existential type representing one of the soda data types.
data SodaType where
    MkSodaType :: SodaClass a => a -> SodaType

-- SoSQL functions
-- TODO

-- Paging functionality
-- TODO
