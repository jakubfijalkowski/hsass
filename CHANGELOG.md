# Change Log
All notable changes to this project will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/).

## [0.2.0] - 2015-06-01
### Changed
- Return type of 'compileString' and 'compileFile' is now polymorphic - may
  return both 'String' and 'SassExtendedResult' on success.
- Fixes in documentation (articles, mostly).

### Added
- 'SassExtendedResult' with a compiled string, a list of files included during
  compilation and a source map.

## 0.1.0 - 2015-04-11
### Added
- Support for functions, importers, headers and sass values.
- Compilation of files and strings.

[0.2.0]: https://github.com/jakubfijalkowski/hsass/compare/v0.1.0...v0.2.0
