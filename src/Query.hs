{-# LANGUAGE OverloadedStrings #-}
module Query
    ( 
    ) where

{- Notes:

- Need to create smart constructors.
-
- I'll have to think through how subqueries will be done. Might be a little complicated.
-
- The logic seems very similar to a language. Abstract syntax tree, etc. I suppose that seems kind of obvious because it's the Socrata Query *Language* which is based on SQL, another language.

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

type Column = String
-- Could change content in the future because we can enforce it to follow the given datatypes
type Content = String

-- Obviously completely untrue, but I'll use it to keep track of where I want it to be true.
type NonNegative = Int

-- Need to somehow limit it so that there's only one where clause per query. Possibly other similar limits
--  Could do something "clever" if that's one of the few restrictions by turning it into a different datatype after a where is added, but that seems problematic

data Sorting = ASC | DESC

-- Possibly be more specific in the types like "Column" or something.
-- Need to account for negative limit, which doesn't make sense, somehow.
data Clause = Filter Column Content
    | Select Column 
    | Where String 
    | Order Column (Maybe Sorting)
    | Group String 
    | Having String 
    | Limit NonNegative 
    | Offset NonNegative 
    | Q String 
    | SubQuery Query 
    | Bom Bool

limit :: Int -> Maybe Clause
limit int
    | int < 0 = Nothing
    | otherwise = Just (Limit int)

offset :: Int -> Maybe Clause
offset int
    | int < 0 = Nothing
    | otherwise = Just (Offset int)

-- Could maybe turn Clause into Query and then have a constructor which is recursive?
newtype Query = Query { getClauses :: [Clause] }

-- Just don't export this one I guess.
addClause :: Clause -> Query -> Query
addClause clause (Query clauses) = Query (clause : clauses)

data QueryError = MultipleWith

-- Turn this into monadplus or monoid or whatever in the future.
-- addQuery :: Clause -> Query -> Either QueryError Query

-- SoSQL functions
--
-- Paging functionality
--
