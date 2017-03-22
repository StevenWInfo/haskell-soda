# Table of Contents

- [Query Structure](#query-structure)
- [Query Elements](#query-elements)
- [A Few Extra Notes](#a-few-extra-notes)
- [Soda Responses](#soda-responses)
- [Tips](#tips)
    - [Common Mistakes](#common-mistakes)
- [More Examples](#more-examples)

# Documentation

You can find the official documentation for the API itself at the [Socrata website](https://dev.socrata.com/).

Besides the overview given below, you can also look at the [Haddock documentation](http://stevenw.info/haskell-soda/unreleased) for a more detailed description of the different parts of the library. For those of you very familiar with Haskell, Haddock documentation, and especially GADTs, you can probably go directly there and be able to pick it up pretty quickly. For those that are less familiar with any of those things, reading the following documentation alongside the Haddock documentation will probably be helpful. I'll also be doing some "hand-waving" in the following explanations which might make them less accurate, but easier to intuitively understand. Despite the length and presentation of the documentation, the library isn't too difficult to use. I just need to learn to be more concise and give clearer explanations.

To summarize, the main way to make a call to SODA is by using the `getSodaResponse` function with a possible Application Token, a domain string, a dataset ID string, and a `Query`. To create the query you will construct the different clauses of a query using the different, typed SODA elements. From `getSodaResponse` you'll get back a value of type `Response` which holds values already interpreted into the same Haskell versions of the SODA types which are also used in queries.

## Query Structure

The main part of the query is specified using a value of the [`Query`](http://stevenw.info/haskell-soda/unreleased/Query.html#g:2) type. `Query` is just a record type where every field corresponds to one of the [SoQL Clauses](https://dev.socrata.com/docs/queries/), with the addition of a field for creating simple filters. Due to some name clashes with other functions in Haskell, the field accessor names differ a little, and the actual names are listed in the Haddock documentation for [Query](http://stevenw.info/haskell-soda/unreleased/Query.html). The Haddock documentation also details the overall structure of the Query type. Each field is also a Maybe type to indicate whether any particular clause is even contained in a query. *Note*: the subquery field doesn't actually do anything yet because implementing subqueries will take a little more work which hasn't been done yet.

To construct a query, I recommend starting with the `emptyQuery` value, which has all of the fields set to `Nothing`, and then assigning the desired fields to `Just` some value.

Some of the clauses have some special types of their own which you can learn about how those types are constructed over at the [Query Clauses](http:/stevenw.info/haskell-soda/unreleased/Query.html#g:1) section of the Haddock documentation.

## Query Elements

There are three kinds of elements in a SODA query:

- Values
- Columns (which are sort of like variables)
- Expressions (Like `upper("Hello" || " world")`)

Each of these three kinds of elements represent something during the evaluation of a query and each instance has its own datatype. A value has a type, columns represent values of a single type, and functions represent an expression of a certain type. All of the types these elements can represent are described at the SODA level by the SODA documentation for [datatypes](https://dev.socrata.com/docs/datatypes/). This library creates Haskell equivalents to those types to be used when creating a query. You can find a listing of the equivalent Haskell types at the Haddock documentation for [datatypes](http://stevenw.info/haskell-soda/unreleased/Datatypes.html), where you will also find more information about their structures and properties.

Because values, columns, and expressions are represented differently and have different properties in a query, the Haskell types for these three things differ, even when representing the same SODA type. To create Haskell values of each of these elements, the Haskell types of those values will have to indicate two different things: which of the 3 kinds of query elements it is, and what SODA datatype that part represents. To accomplish this, the library contains several different Haskell types which "wrap around" the Haskell versions of the SODA datatypes.
- `SodaVal` for values
- `Column` for the columns.
- `SodaFunc`, `SodaAgg`, and `SodaOp` for the different functions which make up longer expressions.

The reason the SODA functions are split up into three different types are for other uses in the library, and to break up what would have been a very large type into smaller pieces. They represent general SODA functions, SODA aggregate functions, and SODA operators respectively. A complete list constructors for different SODA functions, which also details the types that all the "function elements" take and produce, is located at the Haddock documentation for [SODA functions](http://stevenw.info/haskell-soda/unreleased/SodaFunctions.html).

Some examples of SODA elements and how they would appear in the parameters of a SODA call:
```haskell
-- 58
walkableDistance = SodaVal (SodaNum 58) :: SodaVal SodaNum

sn = SodaVal . SodaNum :: Double -> SodaVal SodaNum

-- 3
multiplier = sn 3 :: SodaVal SodaNum

-- station
station = Column "station" :: Column Point

-- distance_in_meters(station, 'POINT 45.3 87.2')
stationDistance  = Distance station (SodaVal (Point 45.3 87.2)) :: SodaFunc SodaNum

-- distance_in_meters(station, 'POINT 45.3 87.2') * 3.0 between 7 and 58 + 3
isWalkable       = Between (stationDistance $* multiplier) (sn 7) (walkableDistance $+ sn 3) :: SodaFunc Checkbox
```

The outer type gives you and the compiler the information of whether it's a value, column, or function/expression, and the inner type provides information about what the SODA datatype that the given query part will eventually produce. *Note*: While the values and SodaFunctions are usually able to infer their datatype from the value given to the data constructor, Column needs to have its type declared explicitly because there's nothing in the value given to indicate what type it should be.

### A Few Extra Notes

- The SODA datatypes are also grouped into smaller subsets represented by [several typeclasses](http://stevenw.info/haskell-soda/unreleased/Datatypes.html#g:1). This is to put some constraints on the input element types for some SODA functions. For example, the first parameter of `within_circle(...)` should be geometric types like `Point`, or `Polygon`, but not something like `Text`. Consequently, the corresponding Haskell function `WithinCircle` has the `SodaGeo` typeclass constraint on the first parameter, which only have instances for the geometric SODA datatypes.

- The `case`, `in`, and `not in` SODA functions take a varying amount of arguments which are all heterogenious with respect to the "outer type". This means we have to use a special type called `Expr` to help us construct a heterogenous list of arguments. You'll have to use the `Expr` data constructor on different parts of the input arguments to those functions.

- All of the SODA operators can be constructed with defined operators which are all prefixed with (`$`). Due to some restrictions, some operators such as `AND` had to be changed slightly, so check the [Haddock documentation](http://stevenw.info/haskell-soda/unreleased/SodaFunctions.html#g:1) to use the correct operators. The alterations should be intuitive though. Because you have to specify specific types in Haskell, but SODA operators like `+` can take a mix of any of the numeric SODA types, the resulting type has to be determined somehow, and right now it returns the same "inner type" as the element to the right of the operator.

## SODA Responses

Using the `getSodaBody` function you can send a query to a SODA dataset and get an `IO` response back which has already been interpreted into the Haskell datatypes that that corresponds to the different SODA types. Using the `getSodaBody` function you can send a query to a SODA dataset and you'll get back a value of type `IO Response`. The `Response` type is a list of `Row`s, and a `Row` is a [(Strict) HashMap](https://hackage.haskell.org/package/unordered-containers-0.2.7.2/docs/Data-HashMap-Strict.html) of `Strings` as keys and `ReturnValue` as values.

The `ReturnValue` type is an abstract data type (ADT) which has a different constructor to hold each SODA datatype. For example, a `ReturnType` constructed by the `RPoint` constructor will have type `Point`. Due to a [known issue](https://github.com/StevenWInfo/haskell-soda/issues/28) responses won't include values of SODA `Double` types. `SodaNum` and `Money` return just fine though.

You can also use `getStringBody` to get the response from a SODA query call as a string, if you want to interpret it yourself, or need it in a particular response format.

If something goes wrong between the time the query is sent and received, it will throw an `HttpException` as described by the underlying HTTP library [Req](https://hackage.haskell.org/package/req-0.2.0/docs/Network-HTTP-Req.html#t:HttpException).

## Tips

- Since it's somewhat annoying to put type declarations for `Column` values inline, I recommend defining constants for all of the columns that you are going to use in one batch, and then just using those throughout your code.

- To reduce some of the boilerplate, I recommend making some small functions that combine some of the constructors and other necessary parts. I didn't add these to the library because there could potentially be a lot of them, and it would have been a lot of memorization to use, but they are simple enough to make when they are needed. Similarly, for some of the longer names you can create some shorter constants or function names.

- I actually recommend you *don't* use OverloadedStrings in the same file as queries are being built because sometimes the compiler gets confused with strings and this library.

- Never forget three of the most useful functions in functional programming: map, fold, and filter! If you'd like to do something like create a `MultiPoint` with tuples instead of points, or if you'd like a whole bunch of numbers to be `SodaNum`, then these three functions can make things much more simple and concise.

### Common mistakes

- Don't forget to use `SodaVal` on all values in a query.

- Don't forget to use `Money` and `SodaNum` when creating their respective types.

- Don't forget to use `Expr` on things that need expression type hidden.

- Don't forget to use `Just` on Maybe values like `Query` fields and `Checkbox` values.

## More Examples

The following example is a bit contrived but it queries a dataset with the URL `https://soda.demo.socrata.com/resource/6yvf-kk3n.json?$select=magnitude, region || ' ' || source as region_and_source&$where=region IS NOT NULL AND source IS NOT NULL AND location IS NOT NULL AND within_circle(location, 63.0, -147.0, 60000.0)&$order=magnitude ASC&$limit=3`

It then handles the response to concatenate the magnitude and the `region_and_source` together for all returned rows to get a Haskell list. At the time of writing this, this gives you the following list: `["0.3 magnitude 82km E of Cantwell, Alaska ak", "0.6 magnitude 64km E of Cantwell, Alaska ak", "0.8 magnitude 73km SSW of Delta Junction, Alaska ak"]`
```haskell
import qualified Data.HashMap.Strict as HM

magRegSource = do theResponse <- getSodaResponse Nothing "soda.demo.socrata.com" "6yvf-kk3n" $
    -- Query
    emptyQuery { selects = Just [ Select magnitude, Alias (region $++ SodaVal " " $++ source) "region_and_source"]
               , wheres  = Just . Where $
                    IsNotNull region 
                    $&& IsNotNull source 
                    $&& IsNotNull location
                    $&& WithinCircle location (sn 63) (sn (-147.0)) (sn 60000)
               , orders  = Just $ [Order magnitude ASC]
               , limit   = Just 3
               }
               
     -- Response interpretation
    let mags = map (\x -> HM.lookup "magnitude" x >>= checkNum >>= (Just . (\mag -> mag ++ " magnitude ") . show)) theResponse
    let regSources = map (\x -> HM.lookup "region_and_source" x >>= checkText) theResponse
    return $ zipWith (\mag rs -> fromMaybe "Problem extracting" $ (++) <$> mag <*> rs) mags regSources
    where checkNum (RSodaNum num)    = Just (getSodaNum num)
          checkNum _                 = Nothing
          checkText (RSodaText text) = Just text
          checkText _                = Nothing
```

The next example is even more contrived, but it demonstrates how you can create complex queries reliably. In real applications you may combine many small and varying query elements that come from a myriad of different places in your code. Even though it may be difficult to track where all the different parts come from, and how they are combined, with this library you can still be confident that the result will create a well formed, type-enforced query. Some of the second query, in the example, even comes from information retrieved from the previous query, which is made simpler because the types retrieved from any responses are the same types that are used in the queries.
```haskell
import qualified Data.HashMap.Strict as HM

complexQuery [Select] -> Int -> IO Response
complexQuery inputSelect tolerance = do
    firstResponse <- getSodaResponse Nothing testDomain testDataset $
        emptyQuery { selects = Just [ Select location ]
                   , wheres  = Just . Where $ IsNotNull location $&& IsNotNull magnitude
                   , orders  = Just $ [ Order magnitude DESC ]
                   , limit   = Just 1
                   }
                   
    let maxLocation = safeHead firstResponse >>= HM.lookup "location" >>= checkPoint
    
    secondResponse <- case maxLocation of
        Nothing       -> return []
        Just maxPoint -> getSodaResponse Nothing "odn.data.socrata.com" "h7w8-g2pa" $
                            emptyQuery { selects = Just $ (Select geoid) : inputSelect 
                                       , wheres  = Just . Where $
                                            Intersects the_geom (SodaVal maxPoint)
                                            $|| WithinCircle (Column "the_geom" :: Column MultiPolygon) (sn $ latitude maxPoint) (sn $ longitude maxPoint) (sn $ tolerance)
                                       , limit   = Just 1
                                       }
    return secondResponse
    where location  = Column "location"  :: Column Point
          magnitude = Column "magnitude" :: Column SodaNum
          the_geom  = Column "the_geom"  :: Column MultiPolygon
          intptlat  = Column "intptlat"  :: Column SodaText
          geoid     = Column "geoid"     :: Column SodaText
          sn        = SodaVal . SodaNum

earthquakeInfo = complexQuery [ Select (Column "name" :: Column SodaText) ] inputFunction 1000000
```
