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

### [x] Comparisons in the filters

Support operators like `<`, and `>`, along with the standard `=`:

```
users id>1000
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

### [ ] Support null checks

Not null
```
users expireAt
```

Null
```
users !expireAt
```


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

### [x] Updates

```
caseFiles 1 | set! title=new_title
```

### [x] Deletes

```
caseFiles 1 | delete!
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

### [ ] Functions on values in a condition

```
accessKey key=sha256(xxxxxx)
```

### [x] Self joins

```
folders 42 | folders
```

This can be used as following:

```
-- How many folders that have the height 5:

folders | folders | folders | folders | folders | folders | group: id | s: id, title
```

We are narrowing the results. See the `directionality` section below.

### [ ] Directionality / Specifying the relationship

Some entities are related in different ways and it would be cool to be able to
specify the `directionality`. What I mean by that is it should be possible to
ask the following questions

- `folders` that are owned by `user` : Narrowing down the result set
- `users` that own `folders`         : Expanding the result set

At the moment, our pipe operator `|` chooses a relationship but it should be
possible to specify this somehow. I don't have a syntax for this at the moment but maybe adding the `narrowing`/`expanding` characters as follows might work:


```
Narrowing down the result set : |>
Expanding the result set      : |<
```

So an example could be:

```
users |< folders
```

or

```
folders |> users
```

### [ ] Resource aliases

Allow the following:


```
customers as c 1 => SELECT c.* FROM customers AS c WHERE c.id = 1

```

### [ ] Compose filters/conditions

```
caseFiles 1 | c: title=Sample*
```

Maybe we don't need this since the default operation already supports filters
i.e. `caseFiles title=Sample*` at the moment. But I guess it supports the
incremental calculation model where you can keep on adding operations and don't
have to go back to modify an operation.



### [x] Column aliases

Column aliases
```
customers 1 | s: id as customer_id, name as customer_name
```
should build the following query:

```
SELECT c.id AS customer_id, c.name AS customer_name
  FROM customers AS c
 WHERE c.id = 1
```

## Tasks

### [x] Use instaparse and the [pine grammar](src/pine/pine.bnf) to generate a parser
### [x] Connection pooling
### [x] Create docker image with the pine server
If the db is running locally, use the `ip` that points to the local host. You
can find it by running the following:

```
ip address show dev docker0 | grep 'inet ' | grep -E -o '([0-9.]*)' | head -1
```

### [x] Keep track of the context `entity`
The indexed operations should contain the `entity` as well so that we don't have
to look at previous operations to figure out which entity is being used

### [ ] Enclose strings with quotes
```
customers "Acme Inc."
```

I should start using something like a parsec library and have a formal specification for the syntax at this point.


