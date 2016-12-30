{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Query
    ( Query (..)
    , emptyQuery
    , queryToString
    , queryToParam
    , Filter (Filter)
    , Select (Select, Alias)
    , GroupElem (Groupify)
    , Sorting (ASC, DESC)
    , Order (Order)
    , (===)
    , selectsToParam
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
 -
 - The toParam and replace stuff could be part of a typeclass.
 -
 - I have the column type, but I suppose we could have aliases which aren't columns, but aren't concrete values.
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
data Order where
    Order :: (SodaTypes a) => Column a -> Sorting -> Order

-- |A SODA simple filter as an existential type (to fit into the query type cleanly).
data Filter where
    Filter :: (SodaTypes a, SodaExpr m) => (Column a) -> (m a) -> Filter

-- Selects are a little trickier than just strings. Have to be able to mix with certain functions and things as well as aliases.
-- Maybe type synonym for a list of existential types that are of the SodaExpr type.
data Select where
    Select :: (SodaExpr m, SodaTypes a) => m a -> Select
    Alias  :: (SodaExpr m, SodaTypes a) => m a -> String -> Select

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
                   , orders   :: Maybe [Order]
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
infix 2 ===
(===) :: (SodaTypes a, SodaExpr m) => (Column a) -> (m a) -> Filter
(===) = Filter

-- This might be a monoid? Also, make a show instance for it.
-- Need to figure out how this works.
data QueryError = WhereError

-- |
-- = ToParam functions

-- I don't know if changing ifExists order would make it more performant
-- Need to change up the namings of things.
queryToString :: Query -> UrlParam
queryToString query = intercalate "&" $ map stringify (queryToParam query)
    where stringify (param, val) = param ++ "=" ++ val

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

-- |
-- == ToParam functions of the query parts

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

-- Paging functionality
-- TODO
