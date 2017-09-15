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

## TODO

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
