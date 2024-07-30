# Change Log
All notable changes to this project will be documented in this file. This change
log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [Unreleased]


## [0.8.0] - 2024-07-30
### Added
- Change the context using the `from:` keyword. This is helpful when the tables relations are not linear and look like a tree.
```
company as c | document | from: c | employee
```

### Breaking
- State: `joins` is a vector e.g. `[ "x" "y" ["x" "id" :has "y" "x_id"]]`

### Changed
- State: `join-map` is kept for legacy reasons but it is only used internally.

## [0.7.2] - 2024-07-26
### Changed
- No difference in functionality. Removed a lot of deprecated code - only keeping the code for reborn.

## [0.7.1] - 2024-07-26
### Fixed
- Allow spaces in the start of a pine expression

## [0.7.0] - 2024-07-26
### Added
- Support for `in` operator

### Changed
- Error type is returned. It is either nothing or `parse`.

## [0.6.0] - 2024-07-22
### Added
- Support for directional joins:
```
employee | has: employee
employee | of: employee
employee | employee^
```
- Columns can be qualified by table aliases:
```
employee as e | s: e.name
```

## [0.5.4] - 2024-07-16
### Fixed
- Incorrect hints were generated in case of ambiguity

## [0.5.3] - 2024-07-16
### Fixed
- Incorrect schema being returned in hints when joining from child to parent

## [0.5.2] - 2024-07-14
### Changed
- Default `limit` is `250` if not specified

### Fixed
- All columns weren't being select in some cases e.g. using `company | s: id | employee`, the columns from `employee` table weren't being selected

## [0.5.1] - 2024-07-11
### Added
- Context sensitive columns selection e.g. `company | s: id | employee | s: id`

## [0.5.0] - 2024-07-10

### Added
- Hints can be provided to resolve ambigious joins e.g. instead of `company | employee`, you can explicitly specify the join column i.e. `column | employee .company_id`
- The delete operation uses a nested query. The column used for deletes must be specified:

```pine
public.company | delete! .id
```

evaluates to:

```sql
DELETE FROM
  "public"."company"
WHERE
  "id" IN (
    SELECT
      "c_0"."id"
    FROM
      "public"."company" AS "c_0"
  );
```
- Conditions can be composed. Following are allowed:

```pine
company | where: id='xxx'
company | w: id='xxx'
company | id='xxx'
```

### Changed
- Conditions can't be combined with the tables e.g. `company id='xxx'`. Instead compose them using pipes: `company | id='xxx'`
- Double quotes around strings aren't supported anymore. Use single quotes i.e. instead of `id="xxx"`, use `id='xxx'`

### Removed
- Support for `group`, `order`, `set!` is dropped. It will be added soon in the up coming versions.
- Context sensitive columns selection

## [0.4.8] - 2024-06-24
### Fixed
- Strings can contain a `+` character.

## [0.4.7] - 2024-06-13
### Fixed
- Db host can be configrued using an environment variable: `DB_HOST`

## [0.4.6] - 2024-06-13
### Changed
- The host is returned as the connection id instead of an internal identifier.

## [0.4.5] - 2024-06-13
### Changed
- Updated configuration to require environment variables: `DB_NAME`, `DB_USER`, `DB_PASSWORD`


## [0.4.4] - 2024-06-13
### Fixed
- Support for multiple architectures i.e. amd64 and arm64

## [0.4.3] - 2024-06-11
### Fixed
- It wasn't possible to get the relations between tables using a readonly user.
- Generating an uberjar so that dependencies are not loaded when the server starts.

## [0.4.2] - 2024-05-04
### Added
- The context contains the schema as well.

### Fixed
- The values for the filters weren't being quoted properly in some cases

### Breaking
- The hints for tables contain an object of schema and table instead of just a string i.e. table.

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
