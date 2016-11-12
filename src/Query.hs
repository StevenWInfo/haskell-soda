{-# LANGUAGE OverloadedStrings #-}
module Query
    ( 
    ) where

{- Notes:


 -}

-- SODA datatypes. Might want to move to another file.
-- Numbers, Doubles, and Moneys are numeric types that can interact. Might need to make an instance of numeric or a custom typeclass if I don't want them interacting with other types.
-- Do I want these to be type synonyms or newtypes?

-- Maybe should actually use ternary logic instead?
type Checkbox = Maybe Bool

{-
Checkbox
Double
FloatingTimestamp
Line
Location
Money
MultiLine
MultiPoint
MultiPolygon
Number
Point
Polygon
Text
 -}
--

type Field = String
-- Could change content in the future because we can enforce it to follow the given datatypes
type Content = String

-- Simple filter functionality
type Filter = (Field, Content)
--

-- SoQL functionality
-- Need to somehow limit it so that there's only one where clause per query. Possibly other similar limits
--  Could do something "clever" if that's one of the few restrictions by turning it into a different datatype after a where is added, but that seems problematic

-- Possibly be more specific in the types like "field" or something.
-- Need to account for negative limit, which doesn't make sense, somehow.
data Clause = Select String 
    | Where String 
    | Order String 
    | Group String 
    | Having String 
    | Limit Int 
    | Offset Int 
    | Q String 
    | SubQuery Query 
    | Bom Bool

newtype Query = Query { getClauses :: [QueryElement] }

-- I could either make it freeform like this where you can keep adding whatever to the query, or I could make a constructor which takes three arguments for each type of thing.
-- I'm starting to think the other way might be better because then I have to wrap this type around each every time.
data QueryElement = Simple Filter | SoQL Clause

data QueryError = MultipleWith

-- Turn this into monadplus or monoid or whatever in the future.
-- addQuery :: Clause -> Query -> Either QueryError Query

-- SoSQL functions

--

-- Paging functionality
--
