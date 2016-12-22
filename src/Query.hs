{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Query
    ( 
    ) where

import Data.Time.Calendar
import Data.Time.Clock
import Datatypes

{-|
Elements which make SoQL and other parts of identifying what data you want easier.
 -}

{- #Notes:
-}

-- Could change content in the future because we can enforce it to follow the given datatypes
type Content = String

-- |Placeholder for more complex type later
type Predicate = String

-- Obviously completely untrue, but I'll use it to keep track of where I want it to be true.
type NonNegative = Int

data Sorting = ASC | DESC

-- Could possibly be confused with Ord class.
-- Fix this.
data Order = Order (Column SodaTypeBox) Sorting

-- |A SODA simple filter as an existential type (to fit into the query type cleanly).
data Filter where
    Filter :: (SodaTypes a, SodaExpr m) => (Column a) -> (m a) -> Filter

-- Selects are a little trickier than just strings. Have to be able to mix with certain functions and things as well as aliases.
type Select = String

type Where = String

type Having = String

-- Possibly confusion with the mathematical concept of a group
data GroupElem where
    Groupify :: SodaTypes sodatype => Column sodatype -> GroupElem

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
queryToUrlParameters :: Query -> String
queryToUrlParameters query = filters'
    where filters' = filtersToUrlParameters (filters query)

-- It's weird to take the head as the initial value with foldr, but order doesn't matter and I'm guessing this is more efficient.
filtersToUrlParameters :: [Filter] -> String
filtersToUrlParameters [] = ""
filtersToUrlParameters (first:filters') = foldr addFilter (filterToUrlParameter first) filters'
    addFilter filt accum = accum ++ " " ++ (filterToUrlParameter filt)

filterToUrlParameter :: Filter -> String
filterToUrlParameter (Filter (Column name) (SodaVal val)) = 
-}

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

-- Paging functionality
-- TODO
