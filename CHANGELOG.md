# Changelog

The format is based on [Keep a Changelog](http://keepachangelog.com/) and this project adheres to [Semantic Versioning](http://semver.org/).

## [unreleased](https://github.com/uzh/guardian/tree/HEAD)

## [0.3.3](https://github.com/uzh/guardian/tree/0.3.3) - 2025-04-03

### Fixed

- support for OCaml 5.x

## [0.3.2](https://github.com/uzh/guardian/tree/0.3.2) - 2024-03-21

### Changed

- use string preprocessor instead of defining and using sql functions

### Deprecated

- database `start` function, as no functions need to be defined anymore

## [0.3.1](https://github.com/uzh/guardian/tree/0.3.1) - 2024-01-22

### Added

- role assingment base structure for specifiying which role can assign which ones

### Removed

- old unused database tables

### Fixed

- package deprecation warnings

## [0.3.0](https://github.com/uzh/guardian/tree/0.3.0) - 2023-09-21

### Added

- helper functions on backend for integrated guardian usage

### Changed

- performance update, reduce amount of needed role permissions

### Removed

- relations

## [0.2.0](https://github.com/uzh/guardian/tree/0.2.0) - 2023-07-05

### Added

- helper functions on backend for integrated guardian usage

### Changed

- extract roles into separate table
- rework relations
- combine "find checker" functions
- optimised validate function


## [0.1.1](https://github.com/uzh/guardian/tree/0.1.1) - 2023-03-28

### Added

- test cases for reported bug

### Fixed

- checking for entity and uuid, bug allowed in certain cases to get access when action on entity matched

## [0.1.0](https://github.com/uzh/guardian/tree/0.1.0) - 2023-03-13

💥 The start of using the changelog. Preparations for our version 0.1.0.

### Added

- deployment for new releases and documentation

### Changed

- typified the guardian library
- restructure the api
