# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [Unreleased]
### Fixed
- It wasn't possible to use empty strings in conditions e.g. the following wasn't working:
```
customers industry=""
```
- Setting string values wasn't working e.g.
```
customers 1 | set! industry="Test"
```

## 0.1.0 - 2019-04-21
### Added
- Check out the [features][features] document for a list of features

[Unreleased]: https://github.com/ahmadnazir/pine/compare/0.1.0...HEAD
<!-- [0.1.1]: https://github.com/ahmadnazri/pine/compare/0.1.0...0.1.1 -->
[features]: FEATURES.md
