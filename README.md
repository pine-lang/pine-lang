# pine

## Setup

### Prerequisites

For the mysql plugin, following is required (for `mysql_config`):
```
sudo apt-get install libmysqlclient-dev
```

### Build and run

```
stack build pine
stack exec pine-exe
```

## Examples

```
caseFiles "sample" | documents "test"
caseFiles "sample" | documents *
cf "sample" | docs *
docs "sample" | cf *
cf 1 | docs *
```

## Run tests

```
stack test
```

## Todo

- Add support for following entities:

```
customers
[customerUserMap]
users
signers
signingRequests
folders
```

- Add support for many to many relationships:

Instead of this:

```
customers "Acme" | customerUserMap * | users *
```

it should be possible to skip of the mapping table:

```
customers "Acme" | users *
```

- Generate the schema specific functionality dynamically:
At the moment, some of the Penneo schema is hardcoded. I am clueless about how to achieve this. 

## Issues

### How to keep the history of relevant entities?

Consider the following query:

```
cf 1 | s "Joe" | cf *
```

gives all the case files that has signers "Joe" instead of just "1".

I need to keep a history of all the entities that were searched for. Before making
a new query, go through the history. If no entity is found, then keep looking


### Find entities that are not directly connected?

I need the `isRelatedTo` function along with 2 helper functions:

- x `hasOne`      y
- x `hasMany`     y (inverse of the previous one)
- x `isRelatedTo` y (x show be related to y through a hasMany and hasOne relationship)
