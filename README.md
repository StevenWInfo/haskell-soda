#haskell-soda
Haskell bindings for the Socrata Open Data API (SODA).

*Disclaimer:* This is not an official library from Socrata. There aren't any official Haskell bindings for SODA (or any other unofficial ones that I'm aware of), but if you want to use some official bindings for some other programming languages, you can find them at the SODA documentation page foor [Libraries & SDKs](https://dev.socrata.com/libraries/).

This library is designed to put many of the specifications of SODA into the types of the library. This attempts to constrain the URL - mostly the URL parameters - such that you will have a much greater guarantee at compile time that the API calls of your compiled program will succeed. It is also intended not to restrict any legitimate API calls from being created, but if it does, create a github issue.

As long as the code using this library compiles, you will be very likely to construct a well formed query, even if the pieces of a query are constructed in many disparate other pieces of code. It gives you more confidence that your queries are well formed over string construction in several ways. It gives a strong guarantee that the query will be syntactically correct. This library will always put closing parenthesis when they are needed, will always put the right number of parameters for any functions, and ensures many other structural rules are followed when creating a query. It also gives some moderate semantic guarantees as well. All of the SoQL query [datatypes](https://dev.socrata.com/docs/datatypes/#,) have been encoded into the types that this library uses. This will prevent things like `$where=location = 3 + 'Foo'` from ever being constructed. These guarantees are also made at compile time, so as long as your code compiles you won't have to worry about those problems. No runtime error or exception handling required.

The bindings are currently designed for the 2.1 version of the SODA.

It will probably be quite a while until it is ready for use in any sort of production environment. That could be sped up with some help, though!  I'm still pretty new to creating larger projects with Haskell, so any suggestions or pull requests are more than welcome.

The library requires a lot more boilerplate than I would have liked, but with the higher guarantees that the library gives you, you at least get something for that boilerplate. If you're making queries as part of a hobby project, then this might not be the library you want to use, and you could probably get away with just simply constructing URL strings. However, if you need a higher degree of confidence that your query will be correctly constructed and return values, then this library might be helpful. It might also be more helpful if you are writing a lot of code that combines SODA queries from smaller parts, and don't require long static queries from being written out. We could also possibly reduce the boilerplate in the future with something like Haskell Generics or Template Haskell, although I don't have any specific plans yet.

Currently the library only contains functionality for consumer/query related API calls. It doesn't have any functionality for the publishing side of the API. However, if there's demand for it, then that could be in the plans for the future.

##Documentation

You can find the official documentation at the [Socrata website](https://dev.socrata.com/).

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

For those who are aware of what generalized algebraic data types (GADTs) are, they are used extensively throughout the construction of the different query parts, as well as throughout the library. They are used to put typeclass constraints on the inputs to the `SodaVal` constructor, give a string to the Column constructor but still be able to type it with a SODA type, and to have constructors that represent all of the different functions, and have all of those functions take varying amounts of differently typed parameters, but to only have a single resulting SODA type when constructed.

If you aren't familiar with GADTs, you don't have to worry. The library should hopefully be simple enough to use without having to completely understand them. All you have to know is that some of the type parameters to the type constructors for some types will not always correspond to the possibly varying types of the input parameters to the corresponding data constructors. Sometimes, the type parameter won't even be any of the types of the values given to the data constructor! For example, the `Between` `SodaFunc` constructor takes three arguments with somewhat varying types; let's say `SodaVal SodaNum`, `Column SodaNum`, and `SodaFunc SodaNum`. However, the resulting type of applying `Between` to those values, or any value for that matter, is `SodaFunc Checkbox`. Now you may be saying to yourself "Checkbox? I didn't see any Checkbox type anywhere in the types of the values given to `Between`." Well, you're right. And that's the magic of GADTs. If you would like to know how that magic works, there are resources such as the [Haskell wikibook](https://en.wikibooks.org/wiki/Haskell/GADT) that have more information about GADTs.

(Document that you may need to catch exceptions for 404 and other errors).

(Explain the response type as well).

##Tips

Since it's somewhat annoying to put type declerations for `Column` values inline, I recommend defining constants for all of the columns that you are going to use in one batch, and then just use those throughout your code.

To reduce some of the boilerplate, I recommend making a lot of small functions that combine some of the constructors and other necessary parts. I didn't add these to the library because there could potentially be a lot of them, and it would have been a lot of memorizing to make a lot of them useful, but if you quickly create them one by one, they are simple enough to make when they are needed.

If you can I actually recommend you *don't* use OverloadedStrings in the same file as queries are being built because sometimes the compiler gets confused with strings and this library.

(Some type creation can get kind of long so you can make helper functions to make things shorter. For example, one for points, but then you have to be consistant with longitude and latitude). (Also, if there's anything that is more verbose than you would like, you can usually make it shorter with your own helper functions.)

##Examples

(put some examples closer to the top).

```haskell
category :: Column SodaText
category = Column "category"

item = Column "item" :: Column SodaText

getSodaResponse "data.ct.gov" "y6p2-px98" $ emptyQuery { filters = Just [ category $= SodaVal "Fruit", item $= SodaVal "Peaches"] }
```
