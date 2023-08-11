# Pine

[![Tests](https://github.com/pine-lang/pine-lang/actions/workflows/test.yaml/badge.svg)](https://github.com/pine-lang/pine-lang/actions/workflows/test.yaml)

Pine uses pipes to query a database. When you evaluate the following pine expression:

```
customers name="Acme" | users name="John *" | s: id, email
```

the following SQL is executed:

```
SELECT u.id, u.email
  FROM customers AS c
  JOIN users AS u
    ON (u.customerId = c.id)
 WHERE c.name = "Acme"
   AND u.name LIKE "John %"
```

## Run the pine server

### Config file

```
cp src/pine/config.sample src/pine/config.clj
```

### Run the server

```
./server.sh
```

## Dev

```
./repl.sh
```

## Philosophy

- **Compositional:** operations are composed together to perform a bigger task
- **Incremental:** keep adding operations instead of going back and modify them
- **Hide the plumbing:** let the machine figure out what it can - separate the _what_ from the _how_
- **Concise:** less to type
- **Doesn't replace existing tools:** it reduces the surface area of the underlying platform
