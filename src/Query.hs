{-# LANGUAGE OverloadedStrings #-}
module Query
    ( 
    ) where

{-|
Elements which make SoQL and other parts of identifying what data you want easier.
 -}

{- Notes:

- Need to create smart constructors.
-
- I'll have to think through how subqueries will be done. Might be a little complicated.
-
- The logic seems very similar to a language. Abstract syntax tree, etc. I suppose that seems kind of obvious because it's the Socrata Query *Language* which is based on SQL, another language.

query -> queryinfo

TODO:
- Run hlint and see how much is unnecessary.
- Figure out better names.

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

-- |Placeholder for more complex type later
type Predicate = String

-- Obviously completely untrue, but I'll use it to keep track of where I want it to be true.
type NonNegative = Int

-- Need to somehow limit it so that there's only one where clause per query. Possibly other similar limits
--  Could do something "clever" if that's one of the few restrictions by turning it into a different datatype after a where is added, but that seems problematic

data Sorting = ASC | DESC

-- Possibly be more specific in the types like "Column" or something.
-- Need to account for negative limit, which doesn't make sense, somehow.
-- Don't export limit, offset, or Combine.
-- Is putting the combine there going to make it more difficult to decompose queries?
data Query = Filter Column Content
    | Select Column 
    | Where Predicate 
    | Order Column (Maybe Sorting)
    | Group String 
    | Having String 
    | Limit NonNegative 
    | Offset NonNegative 
    | Q String 
    | SubQuery Query 
    | Bom Bool
    | Combine Query Query

limit :: Int -> Maybe Query
limit int
    | int < 0 = Nothing
    | otherwise = Just (Limit int)

offset :: Int -> Maybe Query
offset int
    | int < 0 = Nothing
    | otherwise = Just (Offset int)

-- This actually can be a monoid.
-- |I'd like to have most of this info at the type level, but I would need to learn more type level programming in Haskell to implement that.
data QueryMeta = QueryMeta { whereExists :: Bool
                           }

-- |Need a better name. Also don't export the constructor.
-- |Also, maybe have a way to have "default values" since I'm usually only changing one thing at a time.
-- This feels so much like a monad, yet I'm not exactly sure how to make the initial context depend on the value. I don't want a return to violate the dependency. Maybe the solution is to bring the metadata up to the type level.
-- |Just makes finding errors more efficient. Sort of like memoization or caching.
data QueryInfo = QueryInfo Query QueryMeta

defaultQueryInfo = { whereExists = False }

queryInfo :: Query -> QueryInfo
queryInfo (Query Where pred) = QueryInfo (Query Where pred) (QueryMeta (defaultQueryInfo { whereExists = True }))
queryInfo query = QueryInfo Query QueryMeta

-- Need to figure out better naming
combineQ :: QueryInfo -> QueryInfo -> Either QueryError QueryInfo
combineQ (QueryInfo q1 qi1) (QueryInfo q2 qi2) = case combineQI qi1 qi2 of
                                                     Right qi -> Right QueryInfo (Combine q1 q2) qi
                                                     Left qe -> Left qe

-- There has got to be a simpler way of writing this (that's still extensible).
combineQI :: QueryMeta -> QueryMeta -> Either QueryError QueryMeta
combineQI qm1 qm2 = { whereExists = ( whereCheck qm1 qm2 ) }
    where whereCheck { whereExists = we1 } { whereExists = we2 } = we1 && we2

-- Reader or writer monad to capture implementation information.
-- WriterT QueryMeta Either QueryError Query
-- Although, maybe not. The problem is creating the WriterT with the restrictions on QueryMeta.

-- This might be a monoid? Also, make a show instance for it.
-- |Should match up with QueryMeta since QueryMeta just makes finding errors more efficient.
data QueryError = { numberWhere :: Int }

-- SoSQL functions
-- TODO

-- Paging functionality
-- TODO
