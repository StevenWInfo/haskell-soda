{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

{-|
    Module      : SodaFunctions
    Description : SODA Query Level Functions
    Copyright   : (c) Steven W
    Maintainer  : Steven W <StevenW.Info@gmail.com>
    Stability   : Unstable

The types for adding in SODA query level functions into a clause or other part of a SoQL query.
-}

module SodaFunctions
    ( SodaFunc (..)
    , SodaAgg (..)
    -- * SODA binary operators
    -- $ops
    , SodaOp
    , ($==)
    , ($&&)
    , ($||)
    , ($<)
    , ($<=)
    , ($>)
    , ($>=)
    , ($+)
    , ($-)
    , ($*)
    , ($/)
    , ($++)
    ) where

import Data.List
import Datatypes

-- Would some more dependent type features have made this simpler?
-- |The regular SODA, query level functions. Use the constructors of this type to add a function into a SODA query.
--
-- You can find all of the documentation about what they do inside of a SODA query at the <https://dev.socrata.com/docs/functions/ SODA documentation>
--
-- Unfortunately declaring all of the typeclasses is pretty verbose, and some constructors have a lot of parameters, so the type declerations of the constructors can get pretty long. Also, there should be some extra constraints on some of them such as only having geometric types as input, but the typeclasses just haven't been made yet.
data SodaFunc datatype where
    -- Need another type constraint on sodaType
    Between                  :: (SodaExpr m, SodaExpr n, SodaExpr o, SodaTypes a) => m a -> n a -> o a -> SodaFunc Checkbox
    Case                     :: (SodaTypes a) => [(Expr Checkbox, Expr a)] -> SodaFunc a
    -- Geo typeclass. I think that it has to be a column, but I'm not sure.
    -- Is this an aggregate or can you put any expression in the input?
    ConvexHull               :: (SodaTypes geo) => Column geo -> SodaFunc MultiPolygon
    DateTruncY               :: (SodaExpr m) => m Timestamp -> SodaFunc Timestamp
    DateTruncYM              :: (SodaExpr m) => m Timestamp -> SodaFunc Timestamp
    DateTruncYMD             :: (SodaExpr m) => m Timestamp -> SodaFunc Timestamp
    Distance                 :: (SodaExpr m, SodaExpr n) => m Point -> n Point -> SodaFunc SodaNum
    -- Takes an agg (can't use in where)
    Extent                   :: (SodaExpr m, SodaTypes geo) => m geo -> SodaFunc MultiPolygon
    -- Input needs to be constrained. This is going to need to be fixed because the list has to all be the same type which is problematic. Maybe if I made all of the different expressions as one type instead? Or maybe I say that you can't put in columns and make a SodaFunc for values?
    In                       :: (SodaExpr m, SodaTypes a) => m a -> [Expr a] -> SodaFunc Checkbox
    Intersects               :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> n b -> SodaFunc Checkbox
    IsNotNull                :: (SodaExpr m, SodaTypes a) => m a -> SodaFunc Checkbox
    IsNull                   :: (SodaExpr m, SodaTypes a) => m a -> SodaFunc Checkbox
    Like                     :: (SodaExpr m, SodaExpr n) => m SodaText -> n SodaText -> SodaFunc Checkbox
    Lower                    :: (SodaExpr m) => m SodaText -> SodaFunc SodaText
    Not                      :: (SodaExpr m) => m Checkbox -> SodaFunc Checkbox
    NotBetween               :: (SodaExpr m, SodaExpr n, SodaExpr o, SodaTypes a) => m a -> n a -> o a -> SodaFunc Checkbox
    -- Input needs to be constrained
    NotIn                    :: (SodaExpr m, SodaTypes a, SodaTypes b) => m a -> [Expr b] -> SodaFunc Checkbox
    NotLike                  :: (SodaExpr m, SodaExpr n) => m SodaText -> n SodaText -> SodaFunc Checkbox
    -- Geo constraint
    NumPoints                :: (SodaExpr m, SodaTypes geo) => m geo -> SodaFunc SodaNum
    -- Alternative geo constraint. Need to test this.
    Simplify                 :: (SodaExpr m, SodaExpr n, SodaTypes geoAlt) => m geoAlt -> n SodaNum -> SodaFunc geoAlt
    -- Alternative geo constraint. Need to test this. Better name?
    SimplifyPreserveTopology :: (SodaExpr m, SodaExpr n, SodaTypes geoAlt) => m geoAlt -> n SodaNum -> SodaFunc geoAlt
    StartsWith               :: (SodaExpr m, SodaExpr n) => m SodaText -> n SodaText -> SodaFunc Checkbox
    -- Num constraint. First parameter might be Agg instead of Column
    -- Is this really an aggregate or can you actually put any expression in the input?
    StdDevPop                :: (SodaTypes num) => Column num -> SodaFunc SodaNum
    -- Num constraint. First parameter might be Agg instead of Column
    StdDevSamp               :: (SodaTypes num) => Column num -> SodaFunc SodaNum
    Upper                    :: (SodaExpr m) => m SodaText -> SodaFunc SodaText
    -- Geo constraint that includes location
    WithinBox                :: (SodaExpr m, SodaExpr n, SodaExpr o, SodaExpr p, SodaExpr q, SodaTypes geo) => m geo -> n SodaNum -> o SodaNum -> p SodaNum -> q SodaNum -> SodaFunc Checkbox
    -- Geo constraint that includes location
    WithinCircle             :: (SodaExpr m, SodaExpr n, SodaExpr o, SodaExpr p, SodaTypes geo) => m geo -> n SodaNum -> o SodaNum -> p SodaNum -> SodaFunc Checkbox
    -- Geo constraint that doesn't include location.
    WithinPolygon            :: (SodaExpr m, SodaExpr n, SodaTypes geo) => m geo -> n MultiPolygon -> SodaFunc Checkbox

-- This needs to be fixed for certain types. It's not always the same as putting the toUrlParam of their parts in the right place.
instance SodaExpr SodaFunc where
    toUrlParam (Between val first last)                         = (toUrlParam val) ++ " between " ++ (toUrlParam first) ++ " and " ++ (toUrlParam last)
    toUrlParam (Case paths)                                     = "case(" ++ (intercalate ", " (map (\(cond, result) -> (exprUrlParam cond) ++ ", " ++ (exprUrlParam result)) paths)) ++ ")"
    toUrlParam (ConvexHull shape)                               = "convex_hull(" ++ toUrlParam shape ++ ")"
    toUrlParam (DateTruncY time)                                = "date_trunc_y(" ++ (toUrlParam time) ++ ")"
    toUrlParam (DateTruncYM time)                               = "date_trunc_ym(" ++ (toUrlParam time) ++ ")"
    toUrlParam (DateTruncYMD time)                              = "date_trunc_ymd(" ++ (toUrlParam time) ++ ")"
    toUrlParam (Distance pointA pointB)                         = "distance_in_meters(" ++ (toUrlParam pointA) ++ ", " ++ (toUrlParam pointB) ++ ")"
    toUrlParam (Extent points)                                  = "extent(" ++ (toUrlParam points) ++ ")"
    toUrlParam (In element values)                              = (toUrlParam element) ++ " IN(" ++ (intercalate ", " (map exprUrlParam values)) ++ ")"
    toUrlParam (Intersects shapeA shapeB)                       = "intersects(" ++ (toUrlParam shapeA) ++ ", " ++ (toUrlParam shapeB) ++ ")"
    toUrlParam (IsNotNull a)                                    = "IS NOT NULL " ++ toUrlParam a
    toUrlParam (IsNull a)                                       = "IS NULL " ++ toUrlParam a
    toUrlParam (Like textA textB)                               = (toUrlParam textA) ++ " like " ++ (toUrlParam textB)
    toUrlParam (Lower text)                                     = "lower(" ++ (toUrlParam text) ++ ")"
    toUrlParam (Not a)                                          = "NOT " ++ toUrlParam a
    toUrlParam (NotBetween val first last)                      = (toUrlParam val) ++ " not between " ++ (toUrlParam first) ++ " and " ++ (toUrlParam last)
    toUrlParam (NotIn element values)                           = (toUrlParam element) ++ " not in(" ++ (intercalate ", " (map exprUrlParam values)) ++ ")"
    toUrlParam (NotLike textA textB)                            = (toUrlParam textA) ++ " not like " ++ (toUrlParam textB)
    toUrlParam (NumPoints points)                               = "num_points(" ++ (toUrlParam points) ++ ")"
    toUrlParam (Simplify geometry tolerance)                    = "simplify(" ++ (toUrlParam geometry) ++ ", " ++ (toUrlParam tolerance) ++ ")"
    toUrlParam (SimplifyPreserveTopology geometry tolerance)    = "simplify_preserve_topology(" ++ (toUrlParam geometry) ++ ", " ++ (toUrlParam tolerance) ++ ")"
    toUrlParam (StartsWith haystack needle)                     = "starts_with(" ++ (toUrlParam haystack) ++ ", " ++ (toUrlParam needle) ++ ")"
    toUrlParam (StdDevPop nums)                                 = "stddev_pop(" ++ (toUrlParam nums) ++ ")"
    toUrlParam (StdDevSamp nums)                                = "stddev_samp(" ++ (toUrlParam nums) ++ ")"
    toUrlParam (Upper text)                                     = "upper(" ++ (toUrlParam text) ++ ")"
    toUrlParam (WithinBox shape nwLat nwLong seLat seLong)      = "within_box(" ++ (toUrlParam shape) ++ ", " ++ (toUrlParam nwLat) ++ ", " ++ (toUrlParam nwLong) ++ ", " ++ (toUrlParam seLat) ++ ", " ++ (toUrlParam seLong) ++ ")"
    toUrlParam (WithinCircle point centerLat centerLong radius) = "within_circle(" ++ (toUrlParam point) ++ ", " ++ (toUrlParam centerLat) ++ ", " ++ (toUrlParam centerLong) ++ ", " ++ (toUrlParam radius) ++ ")"
    toUrlParam (WithinPolygon point multipolygon)               = "within_polygon(" ++ (toUrlParam point) ++ ", " ++ (toUrlParam multipolygon) ++ ")"
    lower sodafunc = Left BadLower

-- |The SODA, query level functions which are aggregates. Seperating them out doesn't actually have any function, but it's good to know which ones are aggregates. It would be nice if there was some way to have $where clauses be able to assert at the type level that they don't have any aggregates, but I can't think of any way to do it.
data SodaAgg datatype where
    Avg   :: SodaTypes a => Column a -> SodaAgg SodaNum
    Count :: SodaTypes a => Column a -> SodaAgg SodaNum
    Max   :: (SodaTypes a) => Column a -> SodaAgg a
    Min   :: (SodaTypes a) => Column a -> SodaAgg a
    Sum   :: Column SodaNum -> SodaAgg SodaNum

instance SodaExpr SodaAgg where
    toUrlParam (Avg col)     = "avg(" ++ (toUrlParam col) ++ ")"
    toUrlParam (Count rows)  = "count(" ++ (toUrlParam rows) ++ ")"
    toUrlParam (Max numbers) = "max(" ++ (toUrlParam numbers) ++ ")"
    toUrlParam (Min numbers) = "min(" ++ (toUrlParam numbers) ++ ")"
    toUrlParam (Sum nums)    = "sum(" ++ (toUrlParam nums) ++ ")"
    lower sodaagg = Left BadLower

-- |Sometimes you need to add parenthesis in a SODA query to assure the correct order of operations. This type allows that.
data Paren datatype where
    Paren :: (SodaExpr m, SodaTypes a) => m a -> Paren a

instance SodaExpr Paren where
    toUrlParam (Paren a) = "(" ++ toUrlParam a ++ ")"
    lower a = Left BadLower

---

-- SODA Operators

-- $ops
--
-- The SODA operators (I forgot what I was going to put here).

-- Equals should actually have the same types on both sides except numeric types.
-- Need to figure out what to do about type ambiguities when there are different types.
-- |The operators provided by SODA. The constructors are hidden and the corresponding infix operators are used instead in order to look more like natural SoQL. This could be included with the SodaFunc type, but that would be a lot of constructors so it's broken out to make smaller and, hopefully, simpler types.
data SodaOp datatype where
    Equals          :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> n b -> SodaOp Checkbox -- Don't export
    NotEquals       :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> n b -> SodaOp Checkbox -- Don't export
    And             :: (SodaExpr m, SodaExpr n) => m Checkbox -> n Checkbox -> SodaOp Checkbox -- Don't export
    Or              :: (SodaExpr m, SodaExpr n) => m Checkbox -> n Checkbox -> SodaOp Checkbox -- Don't export
    Less            :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> n b-> SodaOp Checkbox
    LessOrEquals    :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> n b-> SodaOp Checkbox
    Greater         :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> n b-> SodaOp Checkbox
    GreaterOrEquals :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> n b-> SodaOp Checkbox
    Add             :: (SodaExpr m, SodaExpr n, SodaTypes numA, SodaTypes numB) => m numA -> n numB -> SodaOp numB
    -- Might have to add another similar operator with different characters for final type going the other way.
    Subtract        :: (SodaExpr m, SodaExpr n, SodaTypes numA, SodaTypes numB) => m numA -> n numB -> SodaOp numB
    Multiply        :: (SodaExpr m, SodaExpr n, SodaTypes numA, SodaTypes numB, SodaTypes numC) => m numA -> n numB -> SodaOp numC
    Divide          :: (SodaExpr m, SodaExpr n, SodaTypes numA, SodaTypes numB, SodaTypes numC) => m numA -> n numB -> SodaOp numC
    Concatenate     :: (SodaExpr m, SodaExpr n) => m SodaText -> n SodaText -> SodaOp SodaText

instance SodaExpr SodaOp where
    toUrlParam (Equals a b)          = toUrlParam a ++ " = " ++ toUrlParam b
    toUrlParam (NotEquals a b)       = toUrlParam a ++ " != " ++ toUrlParam b
    toUrlParam (And a b)             = toUrlParam a ++ " AND " ++ toUrlParam b
    toUrlParam (Or a b)              = toUrlParam a ++ " OR " ++ toUrlParam b
    toUrlParam (Less a b)            = toUrlParam a ++ " < " ++ toUrlParam b
    toUrlParam (LessOrEquals a b)    = toUrlParam a ++ " <= " ++ toUrlParam b
    toUrlParam (Greater a b)         = toUrlParam a ++ " > " ++ toUrlParam b
    toUrlParam (GreaterOrEquals a b) = toUrlParam a ++ " >= " ++ toUrlParam b
    toUrlParam (Add a b)             = toUrlParam a ++ " + " ++ toUrlParam b
    toUrlParam (Subtract a b)        = toUrlParam a ++ " - " ++ toUrlParam b
    toUrlParam (Multiply a b)        = toUrlParam a ++ " * " ++ toUrlParam b
    toUrlParam (Divide a b)          = toUrlParam a ++ " / " ++ toUrlParam b
    toUrlParam (Concatenate a b)     = toUrlParam a ++ " || " ++ toUrlParam b
    lower a = Left BadLower

-- Possibly have two equals signs for this and one for filters?
-- |The equals comparison operator as mentioned in the <https://dev.socrata.com/docs/datatypes/number.html SODA documentation>. The infix operator $= was already in use for simple filters so I used the double equals notation that many other languages use for the equality comparison.
infixr 4 $==
($==) :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> n b -> SodaOp Checkbox
($==) = Equals

-- |The not equals comparison operator as mentioned in the <https://dev.socrata.com/docs/datatypes/number.html SODA documentation>.
infixr 4 $!=
($!=) :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> n b -> SodaOp Checkbox
($!=) = Equals

-- |The \"AND\" boolean operator as mentioned in the <https://dev.socrata.com/docs/queries/where.html SODA documentation>. Since you can't use letters in Haskell operators I chose && as the operator since it's similar to the \"AND\" operator used in many other languages.
infixr 3 $&&
($&&) :: (SodaExpr m, SodaExpr n) => m Checkbox -> n Checkbox -> SodaOp Checkbox
($&&) = And

-- |The \"OR\" boolean operator as mentioned in the <https://dev.socrata.com/docs/queries/where.html SODA documentation>. Since you can't use letters in Haskell operators I chose || as the operator since it's similar to the \"OR\" operator used in many other languages.
infixr 2 $||
($||) :: (SodaExpr m, SodaExpr n) => m Checkbox -> n Checkbox -> SodaOp Checkbox
($||) = Or

-- |The less than comparison operator as mentioned in the <https://dev.socrata.com/docs/datatypes/number.html SODA documentation>.
infixr 4 $<
($<) :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> n b -> SodaOp Checkbox
($<) = Less

-- |The less than or equals comparison operator as mentioned in the <https://dev.socrata.com/docs/datatypes/number.html SODA documentation>.
infixr 4 $<=
($<=) :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> n b -> SodaOp Checkbox
($<=) = LessOrEquals

-- |The greater than comparison operator as mentioned in the <https://dev.socrata.com/docs/datatypes/number.html SODA documentation>.
infixr 4 $>
($>) :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> n b -> SodaOp Checkbox
($>) = Greater

-- |The greater than or equals comparison operator as mentioned in the <https://dev.socrata.com/docs/datatypes/number.html SODA documentation>.
infixr 4 $>=
($>=) :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> n b -> SodaOp Checkbox
($>=) = GreaterOrEquals

-- |The addition operator as mentioned in the <https://dev.socrata.com/docs/datatypes/number.html SODA documentation>.
infixl 6 $+
($+) :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> n b -> SodaOp b
($+) = Add

-- |The subtraction operator as mentioned in the <https://dev.socrata.com/docs/datatypes/number.html SODA documentation>.
infixl 6 $-
($-) :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> n b -> SodaOp b
($-) = Subtract

-- |The multiplication operator as mentioned in the <https://dev.socrata.com/docs/datatypes/number.html SODA documentation>.
infixl 7 $*
($*) :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b, SodaTypes c) => m a -> n b -> SodaOp c
($*) = Multiply

-- |The division operator as mentioned in the <https://dev.socrata.com/docs/datatypes/number.html SODA documentation>.
infixl 7 $/
($/) :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b, SodaTypes c) => m a -> n b -> SodaOp c
($/) = Divide

-- |The concatenation operator as mentioned in the <https://dev.socrata.com/docs/datatypes/text.html SODA documentation>. The \"||\" operator used in the actual SODA queries, I used for the OR operator, so I decided to make it similar to the Haskell concatenation operator.
infixr 5 $++
($++) :: (SodaExpr m, SodaExpr n) => m SodaText -> n SodaText -> SodaOp SodaText
($++) = Concatenate
