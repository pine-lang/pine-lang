# pine

Pine is a tool to explore relational datasets. The initial version will enable
the user to use a syntax like follows which will be converted into `SELECT`
statements behind the scenes:

```
customers 1 | users "John"
```

which will be converted into (and execute) the following:

```
SELECT u.*
  FROM customers AS c
  JOIN users AS u
    ON (c.id = u.customerId)
 WHERE c.id = 1
```

The goal is that given a database schema, pine should be able to determine the relationships between the different types of entities so that the user doesn't have to think about them.

## Objectives

- Use a db schema definition as an input
- It should be possible to filter on entities that are not directly connected e.g. if we have the entities: `company`, `employee`, and `skills`, then writing the following should work:

```
company "Acme In" | skills *
```

(give me all the skills that company 'Acme' has to offer)

Note that even if there is no direct relationship between `company` and `skills`, pine should be able to figure out the implicit relationship.

- Many to many relationships

Instead of this:

```
customers "Acme" | customerUserMap * | users *
```

it should be possible to skip of the mapping table:

```
customers "Acme" | users *
```

## Some of the challenges

### How to keep the history of relevant entities?

Consider the following query:

```
company 1 | employee "Joe" | company *
```

gives all the companies that has employees "Joe" instead of just "1".

I need to keep a history of all the entities that were searched for. Before making
a new query, go through the history. If no entity is found, then keep looking


### How to find the entities that are not directly connected?

I need the `isRelatedTo` function along with 2 helper functions:

- x `hasOne`      y
- x `hasMany`     y (inverse of the previous one)
- x `isRelatedTo` y (x show be related to y through a hasMany and hasOne relationship)
