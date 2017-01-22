#haskell-soda

(Table of contents?)

This library provides Haskell bindings for the Socrata Open Data API (SODA). The bindings are currently designed for the 2.1 version of the SODA. Currently the library only contains functionality for consumer/query related API calls. It doesn't have any functionality for the publishing side of the API. However, if there's demand for it, then that could be in the plans for the future.

*Disclaimer:* This is not an official library from Socrata. There aren't any official Haskell bindings for SODA (or any other unofficial ones that I'm aware of), but if you want to use some official bindings for some other programming languages, you can find them at the SODA documentation page foor [Libraries & SDKs](https://dev.socrata.com/libraries/).

The main benefit that this library provides, besides being bindings for Haskell, is that it also gives strong compile-time guarantees that your query is both syntactically and semantically correct, which most, if not all, of the other API bindings to other languages don't give. The structure of the functions make sure that syntactic rules like parenthesis being balanced and the arity of SODA functions are never violated. It also gives some semantic guarantees at compile-time as well. All of the SoQL query [datatypes](https://dev.socrata.com/docs/datatypes/#,) have been encoded into the types that this library uses. This will prevent things like `$where=location = 3 + 'Foo'` from ever being constructed without having to even deal with runtime exception handling.

The library requires more boilerplate and overhead than I would have liked, but you at least get the benefits mentioned above in exchange. If you're making queries as part of a hobby project, then this might not be the library you want to use, and you could probably get away with just simply constructing URL strings. However, if you need a higher degree of confidence that your query will be correctly constructed and return values, then this library might be helpful. It might also be more helpful if you are writing a lot of code that creates SODA queries from combinations of smaller parts, rather than using long static/hardcoded queries. I'm also looking for ways to reduce the boilerplate, but there isn't a specific plan of how to do that yet. You can also reduce some of the boilerplate with some utility functions such as those I describe in the tips section of the documentation.

This library is still very new, so it will probably be a while until it is ready for use in any sort of production environment. That could be sped up with some help, though!  I'm still pretty new to creating larger projects with Haskell, so any suggestions, submitted issues, or pull requests are more than welcome.

##Documentation

You can find the official documentation at the [Socrata website](https://dev.socrata.com/).

Besides the outline given below, you can also look at the [Haddock documentation](http://stevenw.info/haskell-soda/0.1.0.0) for a more detailed description of the different parts of the library. For those of you very familiar with Haskell, Haddock documentation and especially GADTs, you can probably go directly there and be able to pick it up pretty quickly. For those that are less familiar with any of those things, reading the following documentation alongside the Haddock documentation will probably be helpful.

To summarize the the main way to make a call to SODA will be by giving a domain string, dataset ID string, and a `Query` which is a custom type. You will construct the different clauses of a query using the different typed SODA elements. You'll get back a value of type `Response` which you can pick apart to get back the values you are querying for in the Haskell interpretation of SODA datatypes, which were also used in the query.

###Query Structure

The main part of the query is specified using a value of the `Query` type. `Query` is just a record type where every field corresponds to one of the [SoQL Clauses](https://dev.socrata.com/docs/queries/), with the addition of a field for creating simple filters. Due to some name clashes with other functions in Haskell, the field accessor names differ a little, so you should probably check out the [Haddock documentation for Query](http:/stevenw.info/haskell-soda/0.1.0.0/Query.html) to see how the field names differ, and the overall structure of the Query type. Note that changing the subquery field isn't actually used in the rest of the library yet, because implementing subqueries will take a little more work that hasn't been done yet. Each field is also a Maybe type to indicate whether any particular clause is even set and added to the query.

To construct a query, I recommend starting with the `emptyQuery` value, which has all of the fields set to `Nothing`, and then assign the desired fields to `Just` some value.

Some of the clauses have some special types of their own, mostly because we need some slightly heterogeneous types to be included in those fields. 

- `Filter`: combines the column and SODA element to represent a filter. It has a utility operator `$=` so you can create a filter by just writing `column $= SodaVal "value"`.

- `Select`: Has a `Select` constructor for specifying normal selects with just a column or other SODA elements, and an `Alias` constructor to also specify, using a string, that SODA element has an alias.

- `Where`: Any Soda element that is, or evaluates to a `Checkbox`. This restriction makes sure it is like a filter, which a WHERE clause is.

- `Having`: Similar to Where, it is constructed with a SODA element that is, or evaluates to a `Checkbox`. The main difference is that this clause can work with aggregate functions, and the WHERE clause cannot. This restriction isn't currently encoded into this library yet, but it is one of the issues I hope to address.

- `Order`: Takes a column to order in the results, as well as an element of the `Sorting` type which just has two constructors to indicate whether it should be ascending or descending.

- `Group`: Takes a column to be grouped for working with aggregates.

###Query Elements

3 types of parts to an element in a SODA query:

- Values
- Columns (which are sort of like variables)
- Expressions (Like `upper("Hello" || " world")`)

All of these things can have any of the types described by the [SODA documentation](https://dev.socrata.com/docs/datatypes/). This library contains Haskell equivalents to those types which you can find a listing of, and what the Haskell structure of each type is at the [Haddock documentation for datatypes](http://stevenw.info/haskell-soda/0.1.0.0/Datatypes.html).

These types hold all the information that we need in order to create values. However, we still need to create columns and expressions, and if they're going to interact with these typed values, like in `upper('foo') || upper('bar')` or `salary + '1000.00'`, then they will have to have types that can interact with those value types. In other words, we will have to be able to indicate that things like the function `upper(...)` will produce something that has the type `Text` and that a column such as `salary` has the type `Money`.

This means that the Haskell type of these components of a query will have to indicate two different things: which of the 3 parts of a SODA query it makes up, and what SODA datatype does that part represent. For this, there are several different types which "wrap around" the SODA datatypes we have already established.

- `SodaVal` for values
- `Column` for the columns
- `SodaFunc`, `SodaAgg`, and `SodaOp` for the different functions which make up longer expressions. They are split up into three different types for other uses in the library, and also to split up what would have been a type with a lot of constructors. They contain general SODA functions, SODA aggregate functions, and SODA operators respectively.

(Give examples of how all of these things are used in construction).

The outer type gives you and the compiler the information of whether it's a value, column, or function/expression, and the inner type gives you information about what the SODA datatype that the given query part will eventually produce.

For those who are aware of what generalized algebraic data types (GADTs) are, they are used extensively throughout the construction of the different query parts, as well as throughout the library. If you aren't familiar with GADTs, you don't have to worry. The library should hopefully be simple enough to use without having to completely understand them. The only things you will probably notice are that some things that are data constructors in the library's code produce types you may not expect, and that sometimes it may seem like those constructors will make some types seemingly "disappear" (in the same way as existential types if you're familiar with those). If you just consider those data constructors as more like "regular" functions though, it may be a little simpler. If you would like to know more about how GADTs work, though, there are resources such as the [Haskell wikibook](https://en.wikibooks.org/wiki/Haskell/GADT) that are helpful.

In order to make the code look more like the actual queries they represent, all of the SODA operators can be created using custom Haskell operators. All of the operators are prefixed with (`$`). Due to some restrictions, such as the fact that Haskell can only make symbols out of non-alphanumeric characters, some operators such as `AND` had to be changed slightly, so check the [Haddock documentation](http://127.0.0.1:8000/haskell-soda/0.1.0.0/SodaFunctions.html#g:1) to use the correct operators. The alterations should be intuitive though.

###SODA Responses

Using the `getSodaBody` function you can send a query to a SODA dataset and get an `IO` response back interpreted into the Haskell datatypes that we created for the different SODA types. When you send out a query, you'll get back a value of type `IO Response` which is just an `IO` list of `Row`s which are in turn lists of `(String, ReturnValue)` tuples. The `Row`s are just specific association lists which means you can access them with functions like `lookup`.

The `ReturnValue` type is an abstract data type (ADT) that is just an encoding of all of the different SodaTypes into one type with a different constructor for each type. A `ReturnType` constructed by the `RPoint` constructor will have type `Point`.

In summary, your average query to SODA will return a `Response` filled with `Row`s, out of which you can select one, `lookup` a key/field in that row, and check the `ReturnType` for the value of the type you were expecting.

You can also use `getStringBody` to get the response from a SODA query call as a string.

If something goes wrong with the query, it will throw an `IO` exception. Most likely for a 400 if the query wasn't constructed correctly for the dataset, or if the query isn't well formed. That or either a 404 or a 500 response.

##Tips

- Since it's somewhat annoying to put type declerations for `Column` values inline, I recommend defining constants for all of the columns that you are going to use in one batch, and then just use those throughout your code.

- To reduce some of the boilerplate, I recommend making some small functions that combine some of the constructors and other necessary parts. I didn't add these to the library because there could potentially be a lot of them, and it would have been a lot of memorizing to make a lot of them useful, but if you quickly create them one by one, they are simple enough to make when they are needed. Similarly, for some of the longer names you can create some shorter constant or function names.

- If you can, I actually recommend you *don't* use OverloadedStrings in the same file as queries are being built because sometimes the compiler gets confused with strings and this library.

- Never forget three of the most functions in functional programming: map, fold, and filter! If you'd like to do something like create a `MultiPoint` with tuples instead of points, or if you'd like a whole bunch of numbers to be `SodaNum`, then these three functions can make things much simpler and more concise.

###Common mistakes

- Don't forget to put `SodaVal` on all values in a query.

- Don't forget to put `Money` and `SodaNum` on their respective types.

- Don't forget to use `Expr` on things that need expression type hidden.

- Don't forget to put `Just` on Maybe values like `Query` fields and `Checkbox`.

##Examples

(put some examples closer to the top).

```haskell
category = Column "category" :: Column SodaText
item     = Column "item"     :: Column SodaText

response = getSodaResponse "data.ct.gov" "y6p2-px98" $
    emptyQuery { filters = Just [ category $= SodaVal "Fruit", item $= SodaVal "Peaches"] }
```

Is the way to create the following query https://data.ct.gov/resource/y6p2-px98.json?category=Fruit&item=Peaches

(Create some examples that show off when it would be most useful. Things like wanting to be very sure it will work, and constructing queries from smaller parts. Maybe make an example where it would be ambiguous and difficult to know what the other parts of the application are putting into the query, but that you can be confident will work with this library. Things like getting unknown types and values from functions, but as long as their types line up it should be fine.)

```haskell
foo :: SodaType a => SodaVal a -> [Column a] -> [Filter]
foo val columns = map (($=) val) columns

bar :: () -> IO Response

```

(Probably need examples that actually use the response too).
