# Change Log
All notable changes to this project will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/).

## [0.4.0] - 2015-12-19
### Added
- Support for combining `SassValues` (new in hlibsass 0.1.5)

### Changed
- Requires hlibsass version 0.1.5
- `SassValue` derives `Show`

## [0.3.0] - 2015-07-10
### Added
- Support for a `ByteString` result (thanks to [Andy
  Morris](https://github.com/jakubfijalkowski/hsass/pull/3) ).

### Changed
- `SassExtendedResult` is now parametrised by a base result type (e.g.
  `String`).
- The package depends on `transformers` instead of `mtl`.

### Fixed
- Fix GHC 7.10.1 warnings related to AMP proposal.
- Support for stack's lts-2.17 resolver.

## [0.2.0] - 2015-06-01
### Changed
- Return type of `compileString` and `compileFile` is now polymorphic - may
  return both `String` and `SassExtendedResult` on success.
- Fixes in documentation (articles, mostly).

### Added
- `SassExtendedResult` with a compiled string, a list of files included during
  compilation and a source map.

## 0.1.0 - 2015-04-11
### Added
- Support for functions, importers, headers and sass values.
- Compilation of files and strings.

[0.2.0]: https://github.com/jakubfijalkowski/hsass/compare/v0.1.0...v0.2.0
[0.3.0]: https://github.com/jakubfijalkowski/hsass/compare/v0.2.0...v0.3.0
[0.4.0]: https://github.com/jakubfijalkowski/hsass/compare/v0.3.0...v0.4.0
