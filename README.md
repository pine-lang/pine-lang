# Pine

[![Build Status](https://travis-ci.org/ahmadnazir/pine.svg?branch=master)](https://travis-ci.org/ahmadnazir/pine)

Pine uses pipes to query a database. When you evaluate the following pine expression:

```
customers 1 | users name=John* | s: id, email
```

the following SQL is executed:

```
SELECT u.id, u.email
  FROM customers AS c
  JOIN users AS u
    ON (u.customerId = c.id)
 WHERE c.id = 1
   AND u.name LIKE "John*"
```

## Run the pine server

```
UID=${UID}  \
GID=${UID}  \
USR=${USER} \
 docker-compose up
```

## Philosophy

- **Compositional**                  : operations are composed together to perform a bigger task
- **Incremental calculations**       : keep adding operations instead of going back and modify them
- **Hide the plumbing**              : let the machine figure out what it can
- **Concise**                        : less to type
- **Doesn't replace existing tools** : it reduces the surface area of the underlying platform

## Features

### [x] Support multiple filters (AND)

Multiple filters seperated by a whitespace:

```
users name="John" email="john*"
```

### [x] Select specific columns

The following pine expression to select specific columns:
```
customers name=Acme | select: id | users name=John | select: email
```
or even shorter:
```
customers name=Acme | s: id | users name=John | s: email
```
should build the following query:

```
SELECT c.id, u.email
  FROM customers AS c
  JOIN users AS u
    ON (c.id = u.customerId)
 WHERE c.name = "Acme"
   AND u.name = "John"
```

### [ ] Unselect specific columns

The following pine expression to select specific columns:
```
customers name=Acme | unselect: id
```
should build the following query:

```
SELECT c.name -- everything but id
  FROM customers AS c
 WHERE c.id = 1
```

### [x] Support for limit

```
users | l: 10
```

or

```
users | limit: 10
```

### [x] Functions on a column

```
customers | count: id
```

evaluates to:

```
SELECT count(c.id)
  FROM customers AS c
```

### [x] Group on a column

```
customers | count: id | group: status
```

evaluates to:

```
  SELECT count(c.id)
    FROM customers AS c
GROUP BY c.status
```

```
customers name=dummy* | s: status | count: status | group: status
```

should evaluate something like:

```
SELECT c.status, count(c.status)
  FROM customers AS c
 WHERE c.name LIKE "dummy%"
 GROUP BY c.status
```

### [x] Order by a column

```
caseFiles | order: id
```

evaluates to:

```
SELECT cf.*
FROM caseFiles AS cf
ORDER BY cf.id DESC
```

For ascending order:

```
caseFiles | order: +id
```

evaluates to:

```
SELECT cf.*
FROM caseFiles AS cf
ORDER BY cf.id ASC
```

Concise form can also be used:

```
caseFiles | o: id
```

Multiple columns are not supported yet.

### [x] Meta function: references

```
customers | references?
```

to

```
SHOW CREATE TABLE customers
```

### [ ] Support multiple database schemas
For now, the API runs with one predefined database name.

### [ ] Meta function - getting inverse relationships

Also, need to find inverse relationships i.e.

```
customers | references?
```

should find all the tables and the references that point to `customers`

### [ ] Support multiple filters (OR)

Multiple filters seperated by a comma `,`:

```
users name="John", email="john*"
users 1,2
```

### [ ] Figure out relationships between entities

Automatically figure out the relationship between entities if they are not directly related.

Consider the relationship between tables: `customer` has many `users` where each
of them have an `address`. A pine expression as following should be able to
generate a result:

```
customers "Acme Inc" | address "xyz"
```

`customer` is not directly linked to `address` but I want to get a result and also the other way around. A way to do this is to:

1. Convert the schema into a graph where each table is a node
2. Find the shortest distance between to nodes
3. Expand the pine expression to include the nodes not explicitly mentioned in the expression
4. Evaluate the expression for great profit and fun!

In case we find multiple paths with an equal distance, then I need to find a priority algorithm.. More on that later.

### [ ] Updates

```
caseFiles 1 | set! title=new_title
```

### [x] Deletes

```
caseFiles 1 | delete!
```

### [ ] Functions on values in a condition

```
accessKey key=sha256(xxxxxx)
```

### [ ] Support for aliases

Allow the following:


```
c 1 => SELECT c.* FROM customers AS c WHERE c.id = 1

```

### [ ] Compose filters/conditions

```
caseFiles 1 | c: title=Sample*
```

Maybe we don't need this since the default operation already supports filters
i.e. `caseFiles title=Sample*` at the moment. But I guess it supports the
incremental calculation model where you can keep on adding operations and don't
have to go back to modify an operation.


## Tasks

### [x] Use instaparse and the [pine grammar](src/pine/pine.bnf) to generate a parser
### [x] Connection pooling
### [x] Create docker image with the pine server
If the db is running locally, use the `ip` that points to the local host. You
can find it by running the following:

```
ip address show dev docker0 | grep 'inet ' | grep -E -o '([0-9.]*)' | head -1
```

### [ ] Enclose strings with quotes
```
customers "Acme Inc."
```

I should start using something like a parsec library and have a formal specification for the syntax at this point.


