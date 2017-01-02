{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

{-|
    Module      : Query
    Description : SODA Query Builders
    Copyright   : (c) Steven W
    Maintainer  : Steven W <StevenW.Info@gmail.com>
    Stability   : Unstable

Elements which make SoQL and other parts of identifying what data you want easier.
-}

module Query
    ( Query (..)
    , emptyQuery
    , queryToString
    , queryToParam
    , Filter (Filter)
    , Select (Select, Alias)
    , Sorting (ASC, DESC)
    , Order (Order)
    , GroupElem (Groupify)
    , (===)
    ) where

import Data.Function ((&))
import Data.List
import Data.Time.Calendar
import Data.Time.Clock
import Datatypes
import SodaFunctions

{-
#Notes:
 - The toParam stuff could be part of a typeclass.
 -
 - I have the column type, but I suppose we could have aliases which aren't columns, but aren't concrete values.
 -}

-- |Obviously completely untrue, but I'll use it to keep track of where I want it to be true.
type NonNegative = Int

-- |The type of a simple filter query part.
data Filter where
    Filter :: (SodaTypes a, SodaExpr m) => (Column a) -> (m a) -> Filter

-- |The type of the $select query part.
data Select where
    Select :: (SodaExpr m, SodaTypes a) => m a -> Select
    Alias  :: (SodaExpr m, SodaTypes a) => m a -> String -> Select

-- |Used with the Order type to indicate if the order of a column should be ascending or descending.
data Sorting = ASC | DESC

-- Could possibly be confused with Ord class.
-- |The type of the $order query part.
data Order where
    Order :: (SodaTypes a) => Column a -> Sorting -> Order

-- Check at runtime for Nothing/null as a literal value, and to make sure there aren't any aggregates.
-- |The type of the $where query part.
-- Where shouldn't be passed a literal null or have any SodaAgg within a compound type.
data Where where
    Where :: (SodaExpr m) => m Checkbox -> Where

-- Not sure if this should differ much from where. Probably have to determine if aggregated at run time.
-- |The type of the $having query part.
data Having where
    Having :: (SodaExpr m) => m Checkbox -> Having

-- Possibly confusion with the mathematical concept of a group
-- Also, for some reason, this is completely inconsistant with the other query parts.
-- |The type of the $group query part.
data GroupElem where
    Groupify :: SodaTypes sodatype => Column sodatype -> GroupElem

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

-- Need to account for negative limit, which doesn't make sense, somehow.
-- |The type of the Query itself.
-- The query is represented as a Haskell record type, and you can add to or modify a query made by the bindings by creating, adding to, and/or modifying a value of type Query.
data Query = Query { filters  :: Maybe [Filter] -- Type with columns and contents
                   , selects  :: Maybe [Select]
                   , wheres   :: Maybe Where -- Is the lowercase where allowed?
                   , orders   :: Maybe [Order]
                   , groups   :: Maybe [GroupElem] -- Depends on the select clause. Also, might need an existential type.
                   , having   :: Maybe Having -- Depends on the group clause. Similar to where clause.
                   , limit    :: Maybe NonNegative
                   , offset   :: Maybe NonNegative
                   , search   :: Maybe String -- |$q parameter
                   , subquery :: Maybe Query
                   , bom      :: Maybe Bool
                   }

-- |A useful value since we're often building queries from nothing.
emptyQuery :: Query
emptyQuery = Query { filters  = Nothing
                     , selects  = Nothing
                     , wheres   = Nothing
                     , orders   = Nothing
                     , groups   = Nothing
                     , having   = Nothing
                     , limit    = Nothing
                     , offset   = Nothing
                     , search   = Nothing
                     , subquery = Nothing
                     , bom      = Nothing
                     }

-- This operator might look too generic. Maybe make it look more specific for filters.
-- Also might be confusing because later might introduce one for equality comparisons in where and having.
-- |A little utility infix operator which can be used to make creating queries look a little more like a natural SODA query.
infix 2 $=
($=) :: (SodaTypes a, SodaExpr m) => (Column a) -> (m a) -> Filter
($=) = Filter

-- I don't know if changing ifExists order would make it more performant
-- Need to change up the namings of things.
-- |Used for getting a query as part of a URL represented with a string. Not recommended to actually use, but provided for the URL string builder and just in case somebody wants it.
queryToString :: Query -> UrlParam
queryToString query = intercalate "&" $ map stringify (queryToParam query)
    where stringify (param, val) = param ++ "=" ++ val

-- Should possibly be hidden after being imported into Soda.hs.
-- |Creates the list of pairs of parameters and values that go into the URL.
queryToParam :: Query -> [(String, String)]
queryToParam query = filters' ++ selects' ++ orders' ++ groups' ++ limit' ++ offset' ++ search' ++ bom'
    where filters' = ifExists filtersToParam $ filters query
          selects' = ifExists selectsToParam $ selects query
          groups'  = ifExists groupsToParam  $ groups  query
          orders'  = ifExists ordersToParam  $ orders  query
          limit'   = ifExists limitToParam   $ limit   query
          offset'  = ifExists offsetToParam  $ offset  query
          search'  = ifExists searchToParam  $ search  query
          bom'     = ifExists bomToParam     $ bom     query
          ifExists f Nothing = []
          ifExists f (Just a)  = f a

-- subqueryToParam actually can't be a recursive call to queryToParam because subqueries are represented differently. Within the subquery I think you can make a recursive call though.

---
--ToParam functions for the query parts

filtersToParam :: [Filter] -> [(String, String)]
filtersToParam filters' = map filterToParam filters'
    where filterToParam (Filter col val) = ((toUrlParam col), (toUrlParam val))

selectsToParam :: [Select] -> [(String, String)]
selectsToParam selects' = [("$select", intercalate ", " $ map selectToParam selects')]
    where selectToParam (Select col) = toUrlParam col
          selectToParam (Alias col alias) = (toUrlParam col) ++ " as " ++ alias

ordersToParam :: [Order] -> [(String, String)]
ordersToParam orders' = [("$order", intercalate ", " $ map orderToParam orders')]
    where orderToParam (Order col sort) = toUrlParam col ++ " " ++ sortParam sort
          sortParam ASC  = "ASC"
          sortParam DESC = "DESC"

groupsToParam :: [GroupElem] -> [(String, String)]
groupsToParam groups' = [("$group", intercalate ", " $ map groupToParam groups')]
    where groupToParam (Groupify col) = toUrlParam col

limitToParam :: NonNegative -> [(String, String)]
limitToParam limit' = [("$limit", show limit')]

offsetToParam :: NonNegative -> [(String, String)]
offsetToParam offset' = [("$offset", show offset')]

searchToParam :: String -> [(String, String)]
searchToParam search' = [("$q", search')]

bomToParam :: Bool -> [(String, String)]
bomToParam True = [("$$bom", "true")]
bomToParam False = [("$$bom", "false")]
