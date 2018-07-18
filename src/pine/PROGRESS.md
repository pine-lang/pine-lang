# Pine

## Requirements

Transform the following:

```
customers "Acme" | users "John"
```

to something like:

```
SELECT u.id, u.name
  FROM customers AS c
  JOIN users AS u
    ON (u.customerId = c.id)
 WHERE 1
   AND c.name LIKE "Acme%"
   AND u.name LIKE "John%"
```

## What is needed for the transformations:

### 1. Convert each operation to a clojure form

The following is converted:
```
customers "Acme"
```

to

```
{:entity :customers, :filter {:name "acme"}}
```

### 2. Use a reduce function with all the clojure forms to create an AST


```
[
  {:entity :customers, :filter {:name "acme"}},
  {:entity :users,     :filter {:name "John"}}
]
```

to:


```
{
  :select ["u.id", "u.name"]
  :from [:customers "c"]
  :join [
    :users "u" ["u.customerId" "c.id"]
  ]
  :where { 
    :conditions ["c.name LIKE ?" "u.id = ?" ]
    :params     ["acme%" "1"]
  }
}
```

### 3. Convert AST to prepared SQL with parameters

## TODO

### [x] Get the columns for the table in the last operations

The following should only return the columns for the user table:

```
customers 1 | user 1
```

### [x] Select all filter

```
customers *
```

### [x] Use the schema to determine the relationships between tables

Currently, I am relying on a convention i.e. table names are always camel case
and the forign keys follow the same convention. This will definitely not hold
true for all schemas.

### [x] It should be possible to place the expressions in any order

The following should work:

```
customers 1 | users *

=>  SELECT u.*
      FROM customers AS c
      JOIN users AS u
        ON (u.customerId = c.id)
     WHERE 1
       AND c.id = 1
```

as well as


```
users acme | customers *

=>  SELECT c.*
      FROM customers AS c
      JOIN users AS u
        ON (u.customerId = c.id)
    WHERE 1
      AND u.name LIKE "John%"
```

The 'JOINS' should be generated correctly


### [ ] Enclose strings with quotes
```
customers "Acme Inc."
```

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

