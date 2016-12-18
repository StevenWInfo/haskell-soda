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

-- Need to: Make a typeclass for possibly all soda functions (or at least all sets of types), then possibly also one that is for all soda types to make Column existentially quantified.

-- Could change content in the future because we can enforce it to follow the given datatypes
type Content = String

-- |Placeholder for more complex type later
type Predicate = String

-- Obviously completely untrue, but I'll use it to keep track of where I want it to be true.
type NonNegative = Int

-- Need to somehow limit it so that there's only one where clause per query. Possibly other similar limits
--  Could do something "clever" if that's one of the few restrictions by turning it into a different datatype after a where is added, but that seems problematic

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

-- Paging functionality
-- TODO
