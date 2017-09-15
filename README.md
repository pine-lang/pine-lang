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
caseFiles sample | documents test
```

## TODO

- Make the filter optional so that the following is allowed:

```
cf sample | docs
```

- Aliases for table names so that the following can be used:

```
cf sample | docs test
```

- Create a pine daemon that has an active database connection and listens for requests

- Add support for following entities:

```
customers
[customerUserMap]
users
signers
signingRequests
folders
```

- Add support for many to many relationships
Instead of this:

```
customers Acme | customerUserMap | users
```

it should be possible to skip of the mapping table:

```
customers Acme | users
```

- Generate the schema specific functionality dynamically:
At the moment, some of the Penneo schema is hardcoded. I am clueless about how to achieve this. 
