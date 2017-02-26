#haskell-soda

This library provides Haskell bindings for the [Socrata Open Data API](https://dev.socrata.com/) (SODA).

##Introduction

*Disclaimer: This is not an official library from Socrata. There currently aren't any official Haskell bindings for SODA (or any other unofficial ones that I'm aware of), but if you want to use the official bindings for another programming language, you can find a list of them at the SODA documentation page for [Libraries & SDKs](https://dev.socrata.com/libraries/).*

The main benefit to using this library, besides being able to make calls to SODA natively in Haskell, is that it gives strong compile-time guarantees that your query is both syntactically and semantically correct, which makes this library unique among the other language bindings for SODA. The structure of the functions assures that syntactic rules, like balanced parenthesis and SODA functions having the right amount of input parameters, are never violated. It also gives semantic guarantees because all of the SoQL query [datatypes](https://dev.socrata.com/docs/datatypes/#,) have been encoded into the types that this library uses, and are used with all values, columns, and SODA functions. This will prevent things like `$where=location = 3 + 'Foo'` from ever being constructed without even having to deal with runtime exception handling.

The library currently requires more boilerplate than I would have liked, but you at least get the benefits mentioned above in exchange. If you're making queries as part of a hobby project, then this might not be the library you want to use, and you could probably get away with just simply constructing URL strings. However, if you need a higher degree of confidence that your query will be correctly constructed, then this library might be helpful. It might also be helpful if you are writing a lot of code that creates SODA queries from combinations of smaller parts, rather than using long, static/hardcoded queries. I'm also looking for ways to reduce the boilerplate, but there isn't a specific plan of how to do that yet. You can currently reduce some of the boilerplate with utility functions such as those I describe in the tips section of the documentation.

These bindings are currently designed for version 2.1 of SODA. The library also currently only contains functionality for consumer/query related API calls; It doesn't have any functionality for the publishing side of the API. However, if there's demand for it, then that could be in the plans for the future.

This library is still very new, so the design might fluctuate a bit initially and it will probably be a while until it is ready for use in any sort of production environment. Getting to production quality could be sped up with some help, though!  I'm still pretty new to creating larger projects with Haskell, so any suggestions, submitted issues, or pull requests are more than welcome.

The following is a short example of a SODA call for the URL `https://data.ct.gov/resource/y6p2-px98.json?category=Fruit&item=Peaches`
```haskell
category = Column "category" :: Column SodaText
item     = Column "item"     :: Column SodaText

response = getSodaResponse Nothing "data.ct.gov" "y6p2-px98" $
    emptyQuery { filters = Just [ category $= SodaVal "Fruit", item $= SodaVal "Peaches"] }
```

##Documentation

The documentation for the library is located in the [doc directory](doc.md). You can also view the [Haddock documentation](http://stevenw.info/haskell-soda/0.1.0.0) for more info. The official documentation for the API itself is located at the [Socrata website](https://dev.socrata.com/).
