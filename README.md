hSass
=====
[![Build Status](https://travis-ci.org/jakubfijalkowski/hsass.svg?branch=master)](https://travis-ci.org/jakubfijalkowski/hsass)
[![Hackage](https://img.shields.io/hackage/v/hsass.svg)](http://hackage.haskell.org/package/hsass)
[![Hackage deps](https://img.shields.io/hackage-deps/v/hsass.svg)](http://packdeps.haskellers.com/reverse/hsass)

hSass is a Haskell wrapper over [hLibsass]. It gives you a higher-level API for compilation of [Sass] files and easy integration into Haskell applications. It is meant to be simple in use yet provide as much functionality as it can.

It is available on [Hackage].

Check out the [hLibsass' README] if you find a problem with the library, as it may be related to the bindings, not hSass itself.

### Documentation

All of the documentation is in code. If you want to read a quick-start guide, check [Sass.hs] or visit [Hackage docs] for more readable format.

### Running tests

This library uses [Hspec] as a testing framework (with `hspec-discover` to generate main file). To run test suite, do:

    stack test

### Copyright

Copyright (c) 2015-2017 Jakub Fijałkowski. See LICENSE for details.

[hLibsass]: https://github.com/jakubfijalkowski/hlibsass "hLibsass"
[Sass]: http://sass-lang.com/ "Sass"
[Hackage]: http://hackage.haskell.org/package/hsass "hSass"
[hLibsass' README]: https://github.com/jakubfijalkowski/hlibsass/blob/master/README.md "hLibsass README"
[Sass.hs]: Text/Sass.hs "Text/Sass.hs"
[Hackage docs]: http://hackage.haskell.org/package/hsass-0.4.1/docs/Text-Sass.html "documentation"
[Hspec]: hspec.github.io "Hspec"
