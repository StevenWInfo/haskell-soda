{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Query
    ( 
    ) where

import Data.Time.Calendar
import Data.Time.Clock

{-|
Elements which make SoQL and other parts of identifying what data you want easier.
 -}

{- #Notes:

- Need to create smart constructors.

- I'll have to think through how subqueries will be done. Might be a little complicated.

- The logic seems very similar to a language. Abstract syntax tree, etc. I suppose that seems kind of obvious because it's the Socrata Query *Language* which is based on SQL, another language.
    - I should actually create a grammar. Not just for queries but internally for the clauses too.
    - Although, the order of the SoQL actually doesn't matter.
    - The order inside where clauses matter though.
    - Also, the order in the $query field matters.

- I need to forget about abstracting it into space and just get something workable done. Then I can worry about abstracting things out.

- Could use simpler types like tuples for points, but I think that this is better. Not sure though.

##TODO:
- Run hlint and see how much is unnecessary.
- Figure out better names.
- Clean up and make things like naming more consistant.

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

data SodaTerm datatype = Col (Column datatype) | Ex (Expr datatype)

-- Need to: Make a typeclass for possibly all soda functions (or at least all sets of types), then possibly also one that is for all soda types to make Column existentially quantified.

-- Perhaps need to restrict this further whereever this is used to exclude things like whitespace. Not sure if should do at value or type level.
data Column datatype where
    Column :: SodaClass sodatype => String -> sodatype -> Column sodatype

-- Could change content in the future because we can enforce it to follow the given datatypes
type Content = String

-- |Placeholder for more complex type later
type Predicate = String

-- Obviously completely untrue, but I'll use it to keep track of where I want it to be true.
type NonNegative = Int

-- Need to somehow limit it so that there's only one where clause per query. Possibly other similar limits
--  Could do something "clever" if that's one of the few restrictions by turning it into a different datatype after a where is added, but that seems problematic

data Sorting = ASC | DESC

-- Possibly confusing with Ord class.
data Order = Order (Column SodaType) Sorting

data Filter = Filter (Column SodaType) Content

-- Selects are a little trickier than just strings. Have to be able to mix with certain functions and things as well as aliases.
type Select = String

type Where = String

type Having = String

-- Possibly confusion with the mathematical concept of a group
data GroupElem where
    Groupify :: SodaClass sodatype => sodatype -> GroupElem

-- Possibly be more specific in the types like "Column" or something.
-- Need to account for negative limit, which doesn't make sense, somehow.
-- Don't export constructor
-- Either have maybes for all of these or have an empty indicator for all types.
-- Custom datatypes for some of these
data Query = Query { filters  :: [Filter] -- Type with columns and contents
                   , selects  :: [Select]
                   , wheres   :: Where -- Is the lowercase where allowed?
                   , order    :: Maybe Order
                   , group    :: [GroupElem] -- Depends on the select clause. Also, might need an existential type.
                   , having   :: Having -- Depends on the group clause. Similar to where clause.
                   , limit    :: NonNegative
                   , offset   :: NonNegative
                   , search   :: String -- |$q parameter
                   , subquery :: Maybe Query
                   , bom      :: Bool
                   }

defaultQuery :: Query
defaultQuery = Query { filters  = []
                     , selects  = []
                     , wheres   = ""
                     , order    = Nothing
                     , group    = [] :: [GroupElem]
                     , having   = ""
                     , limit    = 1000
                     , offset   = 0
                     , search   = ""
                     , subquery = Nothing
                     , bom      = False
                     }

{-
limit :: Int -> Maybe Query
limit int
    | int < 0 = Nothing
    | otherwise = Just (Limit int)

offset :: Int -> Maybe Query
offset int
    | int < 0 = Nothing
    | otherwise = Just (Offset int)
-}

-- Do I still want this?
-- data QueryMeta = QueryMeta { whereExists :: Bool
-- data QueryInfo = QueryInfo Query QueryMeta

{-
-- Need to figure out better naming
combineQ :: QueryInfo -> QueryInfo -> Either QueryError QueryInfo
combineQ (QueryInfo q1 qi1) (QueryInfo q2 qi2) = case combineQI qi1 qi2 of
                                                     Right qi -> Right (QueryInfo (Combine q1 q2) qi)
                                                     Left qe -> Left qe

-- There has got to be a simpler way of writing this (that's still extensible).
-- |If there are multiple errors, this currently only returns the first one it finds. Eventually I'd like it to return all errors.
combineQI :: QueryMeta -> QueryMeta -> Either QueryError QueryMeta
combineQI qm1 qm2 = do
    isWhere <- whereCheck qm1 qm2
    return (QueryMeta { whereExists = isWhere })
-}

-- This might be a monoid? Also, make a show instance for it.
-- Need to figure out how this works.
data QueryError = WhereError

-- SoSQL functions
-- TODO

-- Paging functionality
-- TODO
