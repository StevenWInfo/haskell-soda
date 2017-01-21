#haskell-soda

(Table of contents?)

This library provides Haskell bindings for the Socrata Open Data API (SODA). The bindings are currently designed for the 2.1 version of the SODA. Currently the library only contains functionality for consumer/query related API calls. It doesn't have any functionality for the publishing side of the API. However, if there's demand for it, then that could be in the plans for the future.

*Disclaimer:* This is not an official library from Socrata. There aren't any official Haskell bindings for SODA (or any other unofficial ones that I'm aware of), but if you want to use some official bindings for some other programming languages, you can find them at the SODA documentation page foor [Libraries & SDKs](https://dev.socrata.com/libraries/).

The main benefit that this library provides, besides being bindings for Haskell, is that it also gives strong compile-time guarantees that your query is both syntactically and semantically correct, which most, if not all, of the other API bindings to other languages don't give. The structure of the functions make sure that syntactic rules like parenthesis being balanced and the arity of SODA functions are never violated. It also gives some semantic guarantees at compile-time as well. All of the SoQL query [datatypes](https://dev.socrata.com/docs/datatypes/#,) have been encoded into the types that this library uses. This will prevent things like `$where=location = 3 + 'Foo'` from ever being constructed without having to even deal with runtime exception handling.

The library requires more boilerplate and overhead than I would have liked, but you at least get the benefits mentioned above in exchange. If you're making queries as part of a hobby project, then this might not be the library you want to use, and you could probably get away with just simply constructing URL strings. However, if you need a higher degree of confidence that your query will be correctly constructed and return values, then this library might be helpful. It might also be more helpful if you are writing a lot of code that creates SODA queries from combinations of smaller parts, rather than using long static/hardcoded queries. I'm also looking for ways to reduce the boilerplate, but there isn't a specific plan of how to do that yet. You can also reduce some of the boilerplate with some utility functions such as those I describe in the tips section of the documentation.

This library is still very new, so it will probably be a while until it is ready for use in any sort of production environment. That could be sped up with some help, though!  I'm still pretty new to creating larger projects with Haskell, so any suggestions or pull requests are more than welcome.

##Documentation

You can find the official documentation at the [Socrata website](https://dev.socrata.com/).

###Query Structure

###Query Elements

3 types of parts in a SODA query:

- Values
- Columns (which are sort of like variables)
- Expressions (Like `upper("Hello" || " world")`)

All of these things can have any of the types described by the SODA documentation. This library contains Haskell equivalents to those types.

*Show all the types? Also show their structure?*

These types hold all the information that we need in order to create values. However, we still need to create columns and expressions, and if they're going to interact with these typed values, like in `upper('foo') || upper('bar')` or `salary + '1000.00'`, then they will have to have types that can interact with those value types. In other words, we will have to be able to indicate that things like the function `upper(...)` will produce something that has the type `Text` and that a column such as `salary` has the type `Money`.

This means that the Haskell type of these components of a query will have to indicate two different things: which of the 3 parts of a SODA query it makes up, and what SODA datatype does that part represent. For this, there are several different types which "wrap around" the SODA datatypes we have already established.

- `SodaVal` for values
- `Column` for the columns
- `SodaFunc`, `SodaAgg`, and `SodaOp` for the different functions which make up longer expressions. They are split up into three different types for other uses in the library, and also to split up what would have been a type with a lot of constructors. They contain general SODA functions, SODA aggregate functions, and SODA operators respectively.

(Give examples of how all of these things are used in construction).

The outer type gives you and the compiler the information of whether it's a value, column, or function/expression, and the inner type gives you information about what the SODA datatype that the given query part will eventually produce.

For those who are aware of what generalized algebraic data types (GADTs) are, they are used extensively throughout the construction of the different query parts, as well as throughout the library. If you aren't familiar with GADTs, you don't have to worry. The library should hopefully be simple enough to use without having to completely understand them. The only things you will probably notice are that some things that are data constructors in the library's code produce types you may not expect, and that sometimes it may seem like those constructors will make some types seemingly "disappear" (in the same way as existential types if you're familiar with those). If you just consider those data constructors as more like "regular" functions though, it may be a little simpler. If you would like to know more about how GADTs work, though, there are resources such as the [Haskell wikibook](https://en.wikibooks.org/wiki/Haskell/GADT) that are helpful.

(Note how the operators all begin with a "$" and are similar to the normal operators).

(Not sure if the reference to all of the types, constructors, and functions should be in a seperate section).

(I could also put the documentation in different markdown files in other places in the library).

###SODA Responses

(Document that you may need to catch exceptions for 404 and other errors).

(Explain the response type as well).

###Reference

Once I make the Haddock documentation viewable, I'll probably replace this section and have it point to that.

####SODA datatypes

All of these types are part of the SodaType typeclass. Some are also grouped into subtypeclasses to put additional constraints on some of the SODA functions. You can view the details of how the types are grouped in Datatype.hs.

<dl>
    <dt>`Money`<dt>
    <dd>
        The type that corresponds with [SODA's Money type](https://dev.socrata.com/docs/datatypes/money.html). The precision beyond the hundreths place doesn't make sense for how most currencies, including the U.S. dollar, is represented, but this datatype can represent any currency whose representation's precision is not necessarily restricted to the hundreths place.
    </dd>

    <dt>`Double`<dt>
    <dd>
        The type that corresponds with [SODA's Double type](https://dev.socrata.com/docs/datatypes/double.html). It's just a Haskell Double type.
    </dd>

    <dt>`Number`<dt>
    <dd>
        The type that corresponds with [SODA's Number type](https://dev.socrata.com/docs/datatypes/double.html). It's also currently just a newtype around Double. Number is actually supposed to have arbitrary precision, and is a bit repetative as a double since we already have double, but I wasn't exactly sure how to implement it. We'll have to look around for true arbitrary precision Haskell types.
    </dd>

    <dt>`SodaText`<dt>
    <dd>
        The type that corresponds with [SODA's Text type](https://dev.socrata.com/docs/datatypes/text.html). The difference in the name of the Haskell type and the SODA type is to prevent collisions and confusion with the popular Haskell Text type.
    </dd>

    <dt>`Double`<dt>
    <dd>
        The type that corresponds with [SODA's Floating Timestamp Type](https://dev.socrata.com/docs/datatypes/floating_timestamp.html). The name is a bit different because floating timestamp seemed a bit long. The precision and rounding of this type need improvement.
    </dd>

    <dt>`Point`<dt>
    <dd>
        The type that corresponds with [SODA's Point Type](https://dev.socrata.com/docs/datatypes/point.html). I didn't make it a simple tuple because the order of the longitude and latitude differ a bit in places. Also, this is a bit more descriptive.
    </dd>

    <dt>`MultiPoint`<dt>
    <dd>
        The type that Corresponds with [SODA's Multipoint type](https://dev.socrata.com/docs/datatypes/multipoint.html). It's just a Haskell list of `Point`s
    </dd>

    <dt>`Location`<dt>
    <dd>
        Corresponds with [SODA's Location type](https://dev.socrata.com/docs/datatypes/location.html). According to the SODA documentation, location is a legacy datatype so it is discouraged from being used and some SODA functions available for the point datatype are not available for the location datatype. The constructor is not exported because there the library currently doesn't know how to represent them in the URL. Unless somebody really needs this type, I don't plan on working on it very much.
    </dd>
                   
    <dt>`Line`<dt>
    <dd>
        Corresponds with [SODA's Line type](https://dev.socrata.com/docs/datatypes/line.html). It's just a newtype around a list of `Point`s
    </dd>

    <dt>`MultiLine`<dt>
    <dd>
        Corresponds with [SODA's Multiline type](https://dev.socrata.com/docs/datatypes/multiline.html). Just a Haskell list of `Line` types.
    </dd>

    <dt>`Polygon`<dt>
    <dd>
        Corresponds with [SODA's Polygon type](https://dev.socrata.com/docs/datatypes/polygon.html). It's defined as a newtype around a list of list of `Point`s.
    </dd>

    <dt>`MultiPolygon`<dt>
    <dd>
        Corresponds with [SODA's Multipolygon type](https://dev.socrata.com/docs/datatypes/multipolygon.html). It's defined as a list of `Polygon`s
    </dd>
</dl>

###Soda Functions

##Tips

- Since it's somewhat annoying to put type declerations for `Column` values inline, I recommend defining constants for all of the columns that you are going to use in one batch, and then just use those throughout your code.

- To reduce some of the boilerplate, I recommend making a lot of small functions that combine some of the constructors and other necessary parts. I didn't add these to the library because there could potentially be a lot of them, and it would have been a lot of memorizing to make a lot of them useful, but if you quickly create them one by one, they are simple enough to make when they are needed.

- If you can I actually recommend you *don't* use OverloadedStrings in the same file as queries are being built because sometimes the compiler gets confused with strings and this library.

(Some type creation can get kind of long so you can make helper functions to make things shorter. For example, one for points, but then you have to be consistant with longitude and latitude). (Also, if there's anything that is more verbose than you would like, you can usually make it shorter with your own helper functions.)

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
