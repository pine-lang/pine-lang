<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Features](#features)
    - [[x] Support multiple filters (AND)](#x-support-multiple-filters-and)
    - [[x] Comparisons in the filters](#x-comparisons-in-the-filters)
    - [[x] Select specific columns](#x-select-specific-columns)
    - [[x] Support for limit](#x-support-for-limit)
    - [[x] Functions on a column](#x-functions-on-a-column)
    - [[x] Group on a column](#x-group-on-a-column)
    - [[x] Functions on the dataset](#x-functions-on-the-dataset)
    - [[x] Order by a column](#x-order-by-a-column)
    - [[x] Meta function: references](#x-meta-function-references)
    - [[ ] Support multiple database schemas](#--support-multiple-database-schemas)
    - [[ ] Meta function - getting inverse relationships](#--meta-function---getting-inverse-relationships)
    - [[x] Support null checks](#x-support-null-checks)
    - [[x] Support multiple filters (OR)](#x-support-multiple-filters-or)
    - [[ ] Figure out relationships between entities](#--figure-out-relationships-between-entities)
    - [[x] Updates](#x-updates)
    - [[x] Deletes](#x-deletes)
    - [[x] Unselect specific columns](#--unselect-specific-columns)
    - [[ ] Functions on values in a condition](#--functions-on-values-in-a-condition)
    - [[x] Self joins](#x-self-joins)
    - [[ ] Directionality / Specifying the relationship](#--directionality--specifying-the-relationship)
    - [[ ] Resource aliases](#--resource-aliases)
    - [[ ] Compose filters/conditions](#--compose-filtersconditions)
    - [[x] Column aliases](#x-column-aliases)

<!-- markdown-toc end -->

# Features
## [x] Support multiple filters (AND)

Multiple filters seperated by a whitespace:

```
users name="John" email="john*"
```

## [x] Comparisons in the filters

Support operators like `<`, and `>`, along with the standard `=`:

```
users id>1000
```

## [x] Select specific columns

The following pine expression to select specific columns:
```
customers name="Acme" | select: id | users name="John" | select: email
```
or even shorter:
```
customers name="Acme" | s: id | users name="John" | s: email
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

## [x] Support for limit

```
users | l: 10
```

or

```
users | limit: 10
```

## [x] Functions on a column

```
customers | count: id
```

evaluates to:

```
SELECT count(c.id)
  FROM customers AS c
```

## [x] Group on a column

```
customers | group: id
```

evaluates to:

```
  SELECT id, count(c.id)
    FROM customers AS c
  GROUP BY c.id
```

If you want to specify the group function, use the following syntax:

```
customers | group: status join: id
```

evaluates to:

```
  SELECT status, GROUP_CONCAT(c.id)
    FROM customers AS c
  GROUP BY c.status
```
## [x] Functions on the dataset

This is like the `group` operation. It differs in the fact that the function
gets applied on the complete data set (instead of groups) and all previously
selected columns are ignored. This needs to be the last operation.

```
users | count: id
```

evaluates to:

```
  SELECT GROUP_CONCAT(c.id)
    FROM customers AS c
```

## [x] Order by a column

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

## [x] Meta function: references

```
customers | references?
```

to

```
SHOW CREATE TABLE customers
```

## [ ] Support multiple database schemas
For now, the API runs with one predefined database name.

## [ ] Meta function - getting inverse relationships

Also, need to find inverse relationships i.e.

```
customers | references?
```

should find all the tables and the references that point to `customers`

## [x] Support null checks

Not null
```
users expireAt?
```

Null
```
users !expireAt?
```


## [x] Support multiple filters (OR)

Multiple filters seperated by a comma `,`:

```
users name="John", email="john*"
users 1,2
users id=1,2
```

## [ ] Figure out relationships between entities

Automatically figure out the relationship between entities if they are not directly related.

Consider the relationship between tables: `customer` has many `users` where each
of them have an `address`. A pine expression as following should be able to
generate a result:

```
customers name="Acme Inc" | address street="xyz"
```

`customer` is not directly linked to `address` but I want to get a result and also the other way around. A way to do this is to:

1. Convert the schema into a graph where each table is a node
2. Find the shortest distance between to nodes
3. Expand the pine expression to include the nodes not explicitly mentioned in the expression
4. Evaluate the expression for great profit and fun!

In case we find multiple paths with an equal distance, then I need to find a priority algorithm.. More on that later.

## [x] Updates

```
caseFiles 1 | set! title="new_title"
```

## [x] Deletes

```
caseFiles 1 | delete!
```

## [x] Unselect specific columns

The following pine expression to select specific columns:
```
customers name="Acme" | unselect: id
```
should build the following query:

```
SELECT c.name -- everything but id
  FROM customers AS c
 WHERE c.id = 1
```

## [ ] Functions on values in a condition

```
accessKey key=sha256(xxxxxx)
```

## [x] Self joins

```
folders 42 | folders
```

This can be used as following:

```
-- How many folders that have the height 5:

folders | folders | folders | folders | folders | folders | group: id | s: id, title
```

We are narrowing the results. See the `directionality` section below.

## [ ] Directionality / Specifying the relationship

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

## [ ] Resource aliases

Allow the following:


```
customers as c 1 => SELECT c.* FROM customers AS c WHERE c.id = 1

```

## [ ] Compose filters/conditions

```
caseFiles 1 | c: title="Sample*"
```

Maybe we don't need this since the default operation already supports filters
i.e. `caseFiles title="Sample*"` at the moment. But I guess it supports the
incremental calculation model where you can keep on adding operations and don't
have to go back to modify an operation.



## [x] Column aliases

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

