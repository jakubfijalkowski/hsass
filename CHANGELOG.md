# Change Log
All notable changes to this project will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/).

## [0.7.0] - 2018-04-02
### Changed
- This library uses UTF-8 to encode/decode all `CString`s (paths included!)

## [0.6.0] - 2018-03-24
### Changed
- Switched to `hLibsass` 0.1.7.0 and `libsass` 3.5.2

### Added
- `compileByteString` for `ByteString`-only compilation
- option to strip BOM/@charset with `sassStripEncodingInfo` option (thanks @h-3-0 !)

### Removed
- `clearNativeFunction` and `clearNativeFunctionList` are not needed anymore

## [0.5.0] - 2017-11-01
### Changed
- Split `SassImporter` into `SassImporter` and `SassHeader` (thanks @h-3-0 !)

### Added
- `SassIporter` now takes path to the file that is being imported into (thanks
  @h-3-0)

## [0.4.2] - 2017-08-28
### Added
- Introduce `defaultSassOptions` as default options (thanks @chris-martin !)

## [0.4.1] - 2017-03-19
### Fixed
- Bump `hlibsass` version to 0.1.5.2 (fixes build problem on Cabal 1.24 & Stack
  1.4)

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
[0.4.1]: https://github.com/jakubfijalkowski/hsass/compare/v0.4.0...v0.4.1
[0.4.2]: https://github.com/jakubfijalkowski/hsass/compare/v0.4.1...v0.4.2
[0.5.0]: https://github.com/jakubfijalkowski/hsass/compare/v0.4.2...v0.5.0
[0.6.0]: https://github.com/jakubfijalkowski/hsass/compare/v0.5.0...v0.6.0
[0.7.0]: https://github.com/jakubfijalkowski/hsass/compare/v0.6.0...v0.7.0
