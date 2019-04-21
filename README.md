# Pine

[![Build Status](https://travis-ci.org/ahmadnazir/pine.svg?branch=master)](https://travis-ci.org/ahmadnazir/pine)

Pine uses pipes to query a database. When you evaluate the following pine expression:

```
customers 1 | users name="John *" | s: id, email
```

the following SQL is executed:

```
SELECT u.id, u.email
  FROM customers AS c
  JOIN users AS u
    ON (u.customerId = c.id)
 WHERE c.id = 1
   AND u.name LIKE "John %"
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
