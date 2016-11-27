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
 -}

-- SODA datatypes. Might want to move to another file.
-- Numbers, Doubles, and Moneys are numeric types that can interact. Might need to make an instance of numeric or a custom typeclass if I don't want them interacting with other types.
-- Do I want these to be type synonyms or newtypes?

data Expr datatype where
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
    --Sum             :: Column (Expr Int)

--data Func = Sum (Column (Expr Int)) -- ?
    -- | StartsWith (Column (Expr

class SodaClass a

-- Some of these might have conflicting names.
-- |Corresponds to ternary valued values with Nothing as null.
type Checkbox = Maybe Bool
instance SodaClass (Maybe Bool)

-- Not sure if money is just fixed precision or more complicated. Round until I find a way to use a better type.
newtype Money = Money { getMoney :: Double } deriving (Show)
instance SodaClass Money

-- Maybe make these be not empty.
instance SodaClass Double
--
-- Eventually replace with a better type. Look at the numbers package
newtype Number = Number { getNumber :: Double } deriving (Show)
instance SodaClass Number

type Text = String

type FloatingTimestamp = UTCTime
instance SodaClass FloatingTimestamp

-- I'm actually not completely sure of the precision required here.
-- Perhaps rename to Position and then have point as a type synonym (since I think that is semantically more correct).
data Point = Point { longitude :: Double
                   , latitude  :: Double
                   } deriving (Show)

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

-- |Existential type representing one of the soda data types.
data SodaType where
    MkSodaType :: SodaClass a => a -> SodaType

-- SoSQL functions
-- TODO

-- Paging functionality
-- TODO
