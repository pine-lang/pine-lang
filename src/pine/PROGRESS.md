# Pine

## Requirements

Transform the following:

```
customers "Acme" | users "John"
```

to:

```
SELECT u.id, u.name
  FROM customers AS c
  JOIN users AS u
    ON (c.id = u.customerId)
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
  :where [
    "c.name" "acme"
    "u.name" "John"
  ]
}
```

### 3. Convert AST to query string
