#haskell-soda
Haskell bindings for the Socrata Open Data API (SODA).

*Disclaimer:* This is not an official library from Socrata. There aren't any official Haskell bindings for SODA (or any other unofficial ones that I'm aware of), but if you want to use some official bindings for some other programming languages, you can find them at the SODA documentation page foor [Libraries & SDKs](https://dev.socrata.com/libraries/).

This library is designed to put many of the specifications of SODA into the types of the library. This attempts to constrain the URL - mostly the URL parameters - such that you will have a much greater guarantee at compile time that the API calls of your compiled program will succeed. It is also intended not to restrict any legitimate API calls from being created, but if it does, create a github issue.

You'll hopefully use this library somewhat like an Embedded Domain Specific Language (EDSL). I'm not entirely sure what qualifies as an EDSL, though, and it's mostly based on SODA/SoQL anyways.

These bindings are nowhere near to complete yet, so you can't actually make any calls with it yet. It will also probably be quite a while until it is ready for use in any sort of production environment. That could be sped up with some help, though!

The bindings are currently designed for the 2.1 version of the SODA.

I'm still pretty new to creating larger projects with Haskell, so any suggestions or pull requests are more than welcome.

##Documentation

You can find the official documentation at the [Socrata website](https://dev.socrata.com/).

I'll wait until there are actual somewhat stable and usable parts before adding anything here.

(Mention that I actually recommend you *don't* use OverloadedStrings in the same file as queries are being built because the compiler gets confused).

(Some type creation can get kind of long so you can make helper functions to make things shorter. For example, one for points, but then you have to be consistant with longitude and latitude). (Also, if there's anything that is more verbose than you would like, you can usually make it shorter with your own helper functions.)

##Examples

Again, nothing really usable to use as an example yet. Will need to fill this in later.
