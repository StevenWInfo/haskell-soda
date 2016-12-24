{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Query
    ( Query (..)
    , emptyQuery
    , queryToParam
    , Filter (Filter)
    , replaceFilters
    , replaceLimit
    , replaceOffset
    , replaceSearch
    , replaceBom
    , (===)
    ) where

import Data.Function ((&))
import Data.List
import Data.Time.Calendar
import Data.Time.Clock
import Datatypes

{-|
Elements which make SoQL and other parts of identifying what data you want easier.
 -}

{- #Notes:
 - The replaceFoo names make sense, but don't make for a very good interface.
 -}

-- Could change content in the future because we can enforce it to follow the given datatypes
-- Easily confusable with UParam
type UrlParam = String

-- Obviously completely untrue, but I'll use it to keep track of where I want it to be true.
type NonNegative = Int

data Sorting = ASC | DESC

-- Could possibly be confused with Ord class.
-- Fix this.
-- Either do this with using an existential type, or make itself as an existential type (using an existential type actually seems easier).
data Order = Order (Column SodaTypeBox) Sorting

-- |A SODA simple filter as an existential type (to fit into the query type cleanly).
data Filter where
    Filter :: (SodaTypes a, SodaExpr m) => (Column a) -> (m a) -> Filter

-- Selects are a little trickier than just strings. Have to be able to mix with certain functions and things as well as aliases.
-- Maybe type synonym for a list of existential types that are of the SodaExpr type.
type Select = String

-- Should maybe use the bool SodaType
data Where where
    Where :: (SodaExpr m) => m Checkbox -> Where

-- Not sure if this should differ much from where. Probably have to determine if aggregated at run time.
data Having where
    Having :: (SodaExpr m) => m Checkbox -> Having

-- Possibly confusion with the mathematical concept of a group
data GroupElem where
    Groupify :: SodaTypes sodatype => Column sodatype -> GroupElem

-- Possibly be more specific in the types like "Column" or something.
-- Need to account for negative limit, which doesn't make sense, somehow.
-- Don't export constructor
-- Either have maybes for all of these or have an empty indicator for all types.
-- Custom datatypes for some of these
data Query = Query { filters  :: Maybe [Filter] -- Type with columns and contents
                   , selects  :: Maybe [Select]
                   , wheres   :: Maybe Where -- Is the lowercase where allowed?
                   , order    :: Maybe Order
                   , groups   :: Maybe [GroupElem] -- Depends on the select clause. Also, might need an existential type.
                   , having   :: Maybe Having -- Depends on the group clause. Similar to where clause.
                   , limit    :: Maybe NonNegative
                   , offset   :: Maybe NonNegative
                   , search   :: Maybe String -- |$q parameter
                   , subquery :: Maybe Query
                   , bom      :: Maybe Bool
                   }

emptyQuery :: Query
emptyQuery = Query { filters  = Nothing
                     , selects  = Nothing
                     , wheres   = Nothing
                     , order    = Nothing
                     , groups   = Nothing
                     , having   = Nothing
                     , limit    = Nothing
                     , offset   = Nothing
                     , search   = Nothing
                     , subquery = Nothing
                     , bom      = Nothing
                     }

-- I don't know if changing ifExists order would make it more performant
-- Intercalculate with ampersands.
queryToParam :: Query -> UrlParam
queryToParam query = intercalate "&" $ filter (/="") params
    where params    = [filters', limit', offset', search', bom']
          filters'  = ifExists filtersToUrlParameters $ filters query
          {-
          selects'  = ifExists selectsToParam $ selects query
          wheres'   = ifExists wheresToParam $ wheres query
          order'    = ifExists orderToParam $ order query
          groups'   = ifExists groupsToParam $ groups query
          having'   = ifExists havingToParam $ having query
          subquery' = ifExists subqueryToParam $ subquery query
           -}
          limit'    = ifExists limitToParam $ limit query
          offset'   = ifExists offsetToParam $ offset query
          search'   = ifExists searchToParam $ search query
          bom'      = ifExists bomToParam $ bom query
          ifExists f Nothing = ""
          ifExists f (Just a)  = f a

-- It's weird to take the head as the initial value with foldr, but order doesn't matter and I'm guessing this is more efficient.
filtersToUrlParameters :: [Filter] -> String
filtersToUrlParameters [] = ""
filtersToUrlParameters (first:filters') = foldl' replaceFilters (filterToUrlParameter first) filters'
    where replaceFilters accum filt = accum ++ "&" ++ (filterToUrlParameter filt)

filterToUrlParameter :: Filter -> String
filterToUrlParameter (Filter col val) = (toUrlParam col) ++ "=" ++ (toUrlParam val)

limitToParam :: NonNegative -> UrlParam
limitToParam limit = "$limit=" ++ (show limit)

offsetToParam :: NonNegative -> UrlParam
offsetToParam offset = "$offset=" ++ (show offset)

searchToParam :: String -> UrlParam
searchToParam search = "$q=" ++ search

-- subqueryToParam actually can't be a recursive call to queryToParam because subqueries are represented differently. Within the subquery I think you can make a recursive call though.

bomToParam :: Bool -> UrlParam
bomToParam True = "$$bom=true"
bomToParam False = "$$bom=false"

-- |Helpful to use with Data.Function.(&)
replaceFilters :: [Filter] -> Query -> Query
replaceFilters filters' query = query { filters = (Just filters') }

-- Maybe a good place to enforce non-negative
replaceLimit :: NonNegative -> Query -> Query
replaceLimit limit' query = query { limit = (Just limit') }

-- Maybe a good place to enforce non-negative
replaceOffset :: NonNegative -> Query -> Query
replaceOffset offset' query = query { offset = (Just offset') }

replaceSearch :: String -> Query -> Query
replaceSearch search' query = query { search = (Just search') }

replaceBom :: Bool -> Query -> Query
replaceBom bom' query = query { bom = (Just bom') }

-- This operator might look too generic. Maybe make it look more specific for filters.
-- Also might be confusing because later might introduce one for equality comparisons in where and having.
infix 2 ===
(===) :: (SodaTypes a, SodaExpr m) => (Column a) -> (m a) -> Filter
(===) = Filter

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

-- This might be a monoid? Also, make a show instance for it.
-- Need to figure out how this works.
data QueryError = WhereError

-- Paging functionality
-- TODO
