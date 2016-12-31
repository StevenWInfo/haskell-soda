{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module SodaFunctions
    ( SodaFunc (..)
    , SodaAgg (..)
    , SodaOp ( Not
             , IsNull
             , IsNotNull
             )
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
data SodaFunc datatype where
    Between :: (SodaExpr m, SodaExpr sodaExpr, SodaExpr sodaExprAlt, SodaTypes sodaType) => m sodaType -> sodaExpr sodaType -> sodaExprAlt sodaType -> SodaFunc Checkbox -- Need another type constraint on sodaType
    Case :: (SodaExpr m, SodaExpr n, SodaTypes a) => [(m Checkbox, n a)] -> SodaFunc a -- Can the condition have a checkbox value/
    ConvexHull :: (SodaTypes geo) => Column geo -> SodaFunc MultiPolygon -- Geo typeclass. I think that it has to be a column, but I'm not sure.
    DateTruncY :: (SodaExpr m) => m Timestamp -> SodaFunc Timestamp
    DateTruncYM :: (SodaExpr m) => m Timestamp -> SodaFunc Timestamp
    DateTruncYMD :: (SodaExpr m) => m Timestamp -> SodaFunc Timestamp
    Distance :: (SodaExpr m, SodaExpr n) => m Point -> n Point -> SodaFunc Number
    Extent :: (SodaExpr m, SodaTypes geo) => m geo -> SodaFunc MultiPolygon -- Takes an agg (can't use in where)
    In :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> [n b] -> SodaFunc Checkbox -- Input needs to be constrained
    Intersects :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> n b -> SodaFunc Checkbox
    Like :: (SodaExpr m, SodaExpr n) => m SodaText -> n SodaText -> SodaFunc Checkbox
    Lower :: (SodaExpr m) => m SodaText -> SodaFunc SodaText
    NotBetween :: (SodaExpr m, SodaExpr n, SodaExpr o, SodaTypes a) => m a -> n a -> o a -> SodaFunc Checkbox
    NotIn :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> [n b] -> SodaFunc Checkbox -- Input needs to be constrained
    NotLike :: (SodaExpr m, SodaExpr n) => m SodaText -> n SodaText -> SodaFunc Checkbox
    NumPoints :: (SodaExpr m, SodaTypes geo) => m geo -> SodaFunc Number -- Geo constraint
    Simplify :: (SodaExpr m, SodaExpr n, SodaTypes geoAlt) => m geoAlt -> n Number -> SodaFunc geoAlt -- Alternative geo constraint. Need to test this.
    SimplifyPreserveTopology :: (SodaExpr m, SodaExpr n, SodaTypes geoAlt) => m geoAlt -> n Number -> SodaFunc geoAlt -- Alternative geo constraint. Need to test this. Better name?
    StartsWith :: (SodaExpr m, SodaExpr n) => m SodaText -> n SodaText -> SodaFunc Checkbox
    StdDevPop :: (SodaTypes num) => Column num -> SodaFunc Number -- Num constraint. First parameter might be Agg instead of Column
    StdDevSamp :: (SodaTypes num) => Column num -> SodaFunc Number -- Num constraint. First parameter might be Agg instead of Column
    Upper :: (SodaExpr m) => m SodaText -> SodaFunc SodaText
    WithinBox :: (SodaExpr m, SodaExpr n, SodaExpr o, SodaExpr p, SodaExpr q, SodaTypes geo) => m geo -> n Point -> o Point -> p Point -> q Point -> SodaFunc Checkbox -- Geo constraint that includes location
    WithinCircle :: (SodaExpr m, SodaExpr n, SodaExpr o, SodaExpr p, SodaTypes geo) => m geo -> n Point -> o Point -> p Number -> SodaFunc Checkbox -- Geo constraint that includes location
    WithinPolygon :: (SodaExpr m, SodaExpr n, SodaTypes geo) => m geo -> n MultiPolygon -> SodaFunc Checkbox -- Geo constraint that doesn't include location.

instance SodaExpr SodaFunc where
    toUrlParam (Between val first last) = (toUrlParam val) ++ " between " ++ (toUrlParam first) ++ " and " ++ (toUrlParam last)
    toUrlParam (Case paths) = "case(" ++ (intercalate "," (map (\(cond, result) -> (toUrlParam cond) ++ ", " ++ (toUrlParam result)) paths)) ++ ")"
    toUrlParam (ConvexHull shape) = "convex_hull(" ++ toUrlParam shape ++ ")"
    toUrlParam (DateTruncY time) = "date_trunc_y(" ++ (toUrlParam time) ++ ")"
    toUrlParam (DateTruncYM time) = "date_trunc_ym(" ++ (toUrlParam time) ++ ")"
    toUrlParam (DateTruncYMD time) = "date_trunc_ymd(" ++ (toUrlParam time) ++ ")"
    toUrlParam (Distance pointA pointB) = "distance_in_meters(" ++ (toUrlParam pointA) ++ ", " ++ (toUrlParam pointB) ++ ")"
    toUrlParam (Extent points) = "extent(" ++ (toUrlParam points) ++ ")"
    toUrlParam (In element values) = (toUrlParam element) ++ " IN(" ++ (intercalate "," (map toUrlParam values)) ++ ")"
    toUrlParam (Intersects shapeA shapeB) = "intersects(" ++ (toUrlParam shapeA) ++ ", " ++ (toUrlParam shapeB) ++ ")"
    toUrlParam (Like textA textB) = (toUrlParam textA) ++ " like " ++ (toUrlParam textB)
    toUrlParam (Lower text) = "lower(" ++ (toUrlParam text) ++ ")"
    toUrlParam (NotBetween val first last) = (toUrlParam val) ++ " not between " ++ (toUrlParam first) ++ " and " ++ (toUrlParam last)
    toUrlParam (NotIn element values) = (toUrlParam element) ++ " not in(" ++ (intercalate "," (map toUrlParam values)) ++ ")"
    toUrlParam (NotLike textA textB) =  (toUrlParam textA) ++ " not like " ++ (toUrlParam textB)
    toUrlParam (NumPoints points) = "num_points(" ++ (toUrlParam points) ++ ")"
    toUrlParam (Simplify geometry tolerance) = "simplify(" ++ (toUrlParam geometry) ++ ", " ++ (toUrlParam tolerance) ++ ")"
    toUrlParam (SimplifyPreserveTopology geometry tolerance) = "simplify_preserve_topology(" ++ (toUrlParam geometry) ++ ", " ++ (toUrlParam tolerance) ++ ")"
    toUrlParam (StartsWith haystack needle) = "starts_with(" ++ (toUrlParam haystack) ++ ", " ++ (toUrlParam needle) ++ ")"
    toUrlParam (StdDevPop nums) = "stddev_pop(" ++ (toUrlParam nums) ++ ")"
    toUrlParam (StdDevSamp nums) = "stddev_samp(" ++ (toUrlParam nums) ++ ")"
    toUrlParam (Upper text) = "upper(" ++ (toUrlParam text) ++ ")"
    toUrlParam (WithinBox shape nwLat nwLong seLat seLong) = "whithin_box(" ++ (toUrlParam shape) ++ ", " ++ (toUrlParam nwLat) ++ ", " ++ (toUrlParam nwLong) ++ ", " ++ (toUrlParam seLat) ++ ", " ++ (toUrlParam seLong) ++ ")"
    toUrlParam (WithinCircle point centerLat centerLong radius) = "whithin_circle(" ++ (toUrlParam point) ++ (toUrlParam centerLat) ++ (toUrlParam centerLong) ++ (toUrlParam radius) ++ ")"
    toUrlParam (WithinPolygon point multipolygon) = "within_polygon(" ++ (toUrlParam point) ++ ", " ++ (toUrlParam multipolygon) ++ ")"

data SodaAgg datatype where
    Avg :: SodaTypes a => Column a -> SodaAgg Number
    Count :: SodaTypes a => Column a -> SodaAgg Number
    Max :: (SodaTypes a) => Column a -> SodaAgg a
    Min :: (SodaTypes a) => Column a -> SodaAgg a
    Sum :: Column Number -> SodaAgg Number

instance SodaExpr SodaAgg where
    toUrlParam (Avg col) = "avg(" ++ (toUrlParam col) ++ ")"
    toUrlParam (Count rows) = "count(" ++ (toUrlParam rows) ++ ")"
    toUrlParam (Max numbers) = "max(" ++ (toUrlParam numbers) ++ ")"
    toUrlParam (Min numbers) = "min(" ++ (toUrlParam numbers) ++ ")"
    toUrlParam (Sum nums) = "sum(" ++ (toUrlParam nums) ++ ")"

data Paren datatype where
    Paren :: (SodaExpr m, SodaTypes a) => m a -> Paren a

instance SodaExpr Paren where
    toUrlParam (Paren a) = "(" ++ toUrlParam a ++ ")"

-- Equals should actually have the same types on both sides except numeric types.
-- |The operators provided by SODA. This could be included with the SodaFunc type, but that would be a lot of constructors so it's broken out to make smaller and, hopefully, simpler types.
data SodaOp datatype where
    Equals          :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> n b -> SodaOp Checkbox -- Don't export
    NotEquals       :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> n b -> SodaOp Checkbox -- Don't export
    And             :: (SodaExpr m, SodaExpr n) => m Checkbox -> n Checkbox -> SodaOp Checkbox -- Don't export
    Or              :: (SodaExpr m, SodaExpr n) => m Checkbox -> n Checkbox -> SodaOp Checkbox -- Don't export
    Not             :: (SodaExpr m) => m Checkbox -> SodaOp Checkbox
    IsNull          :: (SodaExpr m, SodaTypes a) => m a -> SodaOp Checkbox
    IsNotNull       :: (SodaExpr m, SodaTypes a) => m a -> SodaOp Checkbox
    Less            :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> n b-> SodaOp Checkbox
    LessOrEquals    :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> n b-> SodaOp Checkbox
    Greater         :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> n b-> SodaOp Checkbox
    GreaterOrEquals :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> n b-> SodaOp Checkbox
    Add             :: (SodaExpr m, SodaExpr n, SodaTypes numA, SodaTypes numB) => m numA -> n numB -> SodaOp Number
    Subtract        :: (SodaExpr m, SodaExpr n, SodaTypes numA, SodaTypes numB) => m numA -> n numB -> SodaOp Number
    Multiply        :: (SodaExpr m, SodaExpr n, SodaTypes numA, SodaTypes numB) => m numA -> n numB -> SodaOp Number
    Divide          :: (SodaExpr m, SodaExpr n, SodaTypes numA, SodaTypes numB) => m numA -> n numB -> SodaOp Number
    Concatenate     :: (SodaExpr m, SodaExpr n) => m SodaText -> n SodaText -> SodaOp SodaText

instance SodaExpr SodaOp where
    toUrlParam (Equals a b)          = toUrlParam a ++ " = " ++ toUrlParam b
    toUrlParam (NotEquals a b)       = toUrlParam a ++ " != " ++ toUrlParam b
    toUrlParam (And a b)       = toUrlParam a ++ " AND " ++ toUrlParam b
    toUrlParam (Or a b)        = toUrlParam a ++ " OR " ++ toUrlParam b
    toUrlParam (Not a)               = "NOT " ++ toUrlParam a
    toUrlParam (IsNull a)            = "IS NULL " ++ toUrlParam a
    toUrlParam (IsNotNull a)         = "IS NOT NULL " ++ toUrlParam a
    toUrlParam (Less a b)            = toUrlParam a ++ " < " ++ toUrlParam b
    toUrlParam (LessOrEquals a b)    = toUrlParam a ++ " <= " ++ toUrlParam b
    toUrlParam (Greater a b)         = toUrlParam a ++ " > " ++ toUrlParam b
    toUrlParam (GreaterOrEquals a b) = toUrlParam a ++ " >= " ++ toUrlParam b
    toUrlParam (Add a b)             = toUrlParam a ++ " + " ++ toUrlParam b
    toUrlParam (Subtract a b)        = toUrlParam a ++ " - " ++ toUrlParam b
    toUrlParam (Multiply a b)        = toUrlParam a ++ " * " ++ toUrlParam b
    toUrlParam (Divide a b)          = toUrlParam a ++ " / " ++ toUrlParam b
    toUrlParam (Concatenate a b)     = toUrlParam a ++ " || " ++ toUrlParam b

-- Possibly have two equals signs for this and one for filters?
infixr 4 $==
($==) :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> n b -> SodaOp Checkbox
($==) = Equals

infixr 3 $&&
($&&) :: (SodaExpr m, SodaExpr n) => m Checkbox -> n Checkbox -> SodaOp Checkbox
($&&) = And

infixr 2 $||
($||) :: (SodaExpr m, SodaExpr n) => m Checkbox -> n Checkbox -> SodaOp Checkbox
($||) = Or

infixr 4 $<
($<) :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> n b -> SodaOp Checkbox
($<) = Less

infixr 4 $<=
($<=) :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> n b -> SodaOp Checkbox
($<=) = LessOrEquals

infixr 4 $>
($>) :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> n b -> SodaOp Checkbox
($>) = Greater

infixr 4 $>=
($>=) :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> n b -> SodaOp Checkbox
($>=) = GreaterOrEquals

infixl 6 $+
($+) :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> n b -> SodaOp Number
($+) = Add

infixl 6 $-
($-) :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> n b -> SodaOp Number
($-) = Subtract

infixl 7 $*
($*) :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> n b -> SodaOp Number
($*) = Multiply

infixl 7 $/
($/) :: (SodaExpr m, SodaExpr n, SodaTypes a, SodaTypes b) => m a -> n b -> SodaOp Number
($/) = Divide

infixr 5 $++
($++) :: (SodaExpr m, SodaExpr n) => m SodaText -> n SodaText -> SodaOp SodaText
($++) = Concatenate
