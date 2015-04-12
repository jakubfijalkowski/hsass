hSass
=====

hSass is a Haskell wrapper over [hLibsass]. It gives you a higher-level API for compilation of [Sass] files and easy integration into Haskell applications. It is meant to be simple in use yet provide as much functionality as it can.

It is available on [Hackage].

### Getting started

    cabal update && cabal install hsass

### Documentation

All of the documentation is in code. If you want to read a quick-start guide, check [Sass.hs] or visit [Hackage docs] for more readable format.

### Running tests

This library uses [Hspec] as a testing framework (with `hspec-discover` to generate main file). To install them, run:

    cabal install --enable-tests --only-dependencies

To run the test suite, do:

    cabal configure --enable-tests && cabal build && cabal test

### Copyright

Copyright (c) 2015 Jakub Fijałkowski. See LICENSE for details.

[hLibsass]: https://github.com/jakubfijalkowski/hlibsass "hLibsass"
[Sass]: http://sass-lang.com/ "Sass"
[Hackage]: http://hackage.haskell.org/package/hsass "hSass"
[Sass.hs]: Text/Sass.hs "Text/Sass.hs"
[Hackage docs]: http://hackage.haskell.org/package/hsass-0.1.0/docs/Text-Sass.html "documentation"
[Hspec]: hspec.github.io "Hspec"
