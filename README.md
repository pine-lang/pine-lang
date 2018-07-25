# Pine

Pine uses pipes to query a database. When you evaluate the following pine expression:

```
customers Acme* | users John
```

the following SQL is executed:

```
SELECT u.
  FROM customers AS c
  JOIN users AS u
    ON (u.customerId = c.id)
 WHERE c.name LIKE "Acme%"
   AND u.name = "John"
```

## TODO

### [ ] Use instaparse and the [pine grammar](src/pine/pine.bnf) to generate a parser

### [ ] Support multiple schemas
For now, the API runs with one predefined database name.

### [ ] Support for aliases

Allow the following:


```
c 1 => SELECT c.* FROM customers AS c WHERE c.id = 1

```

### [ ] Enclose strings with quotes
```
customers "Acme Inc."
```

I should start using something like a parsec library and have a formal specification for the syntax at this point.

### [ ] Automatically figure out the relationship between entities if they are not directly related

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

### [ ] Select specific columns

```
customers name=Acme | select: id | users name=John | select: email
```
should evaluate:

```
SELECT c.id, u.email
  FROM customers AS c
  JOIN users AS u
    ON (c.id = u.customerId)
 WHERE c.name = "Acme"
   AND u.name = "John"
```

### [ ] Group on a column

```
customers name=dummy* | count: status
```

should evaluate something like:

```
SELECT c.status, count(c.status)
  FROM customers AS c
 WHERE c.name LIKE "dummy%"
```
