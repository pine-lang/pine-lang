# Change Log
All notable changes to this project will be documented in this file. This change
log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [Unreleased]
### Added
- The context contains the schema as well.

### Breaking
- The hints for tables contain and object of schema and table instead of just a string i.e. table.

## [0.4.1] - 2023-08-11
### Added
- Better hints i.e. taking into consideration the context e.g. for expression `document | ..`, only tables related to `document` will be suggested. Also only schemas of the related tables will be suggested.

### Changed
- Reverted the change for getting all the columns. Instead of listing all the columns, we are relying on the `*` again. The change was a remnant of bug related to the ordering of the columns which had to do nothing with explicitly specifying the columns.
- The `connection` protocol doesn't expose the `get-schema` method.

### Fixed
- Numbers as parameters wasn't working e.g. `file version>1`

### Breaking
- Dropped support for MySQL.
- All endpoints are prefixed with `/api/v1`


## [0.4.0] - 2023-07-28
### Added
- Disabled CORS
- API endpoint for getting the active connection:
```
GET /connection

{
  ...
  "connection-id": "..."
}

```
- When using `POST /build` the response also includes the `connection-id`, and `params`
- When using `POST /eval` the response also includes the `connection-id`, `query`, and `params`.
- In case of an error, it is handled and the error message is returned in the API response as `error`
- Limited support for showing hints based on the input

### Fixed
- Pine expression build/eval was failing if the db connection isn't initialized
- An error was being thrown when using `uuid` values in the expressions: `operator does not exist: uuid = character varying`
- Order of the columns in the result was sometimes not the same as the order in the query. Also, all columns are explicitly selected in the sql instead of relying on `*`

## [0.3.1] - 2022-02-14
### Added
- API endpoint for building expressions:
```
POST /build
{
  "expression": "user"
}


{
  ...
  "query": "\nSELECT user_0.* FROM \"user\" AS user_0 WHERE true;\n"
}
```

- API endpoint for evaluating expressions:
```
POST /eval
{
  "expression": "user"
}

{
  ...
  "result": [
    {
      "email": "john@acme.com",
      "name": "John Doe",
      ...
    },
    ...
  ]
}
```
- API endpoint for setting the connection:
```
PUT /connection
{
  "connection-id": "default"
}

{
  "connection-id": "default"
}
```
- API endpoint for getting the connections
```
GET /connections

[
  "result": [ "default", "mysql-test" ]
]
```

### Deprecated
- API endpoint for building sql expressions `POST /pine/build`. Use the new endpoint: `POST /build`.

## [0.3.0] - 2022-02-10
### Added
- Support for Postgres

### Breaking changes
- Default limit of `50` is removed for updates and `1` for deletes
- Unselecting of columns is disabled. This will be enabled again in a future release.
```
customers | unselect: id
```
- Format of the config file is changed. This was done to support multiple
  connection configurations. The `:connection-id` property can be set to select
  the default connection.

## [0.2.0] - 2019-04-26
### Added
- Unselecting of columns
```
customers | unselect: id
```

### Fixed
- It wasn''t working:
```
customers industry=""
```
- Setting string values wasn't working e.g.
```
customers 1 | set! industry="Test"
customers 1 | set! industry=123
```

## 0.1.0 - 2019-04-21
### Added
- Check out the [features][features] document for a list of features

[Unreleased]: https://github.com/ahmadnazir/pine/compare/0.3.1...HEAD
[0.3.1]: https://github.com/ahmadnazir/pine/compare/0.3.0...0.3.1
[0.3.0]: https://github.com/ahmadnazir/pine/compare/0.2.0...0.3.0
[0.2.0]: https://github.com/ahmadnazir/pine/compare/0.1.0...0.2.0
[features]: FEATURES.md
