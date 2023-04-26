# Dev setup

Instead of running the server, run the repl:

```
./repl.sh
```

From emacs (using cider), connect to the repl:

```
M-x cider-connect-clj
```

Cider will detect the repl running on localhost. Select it and you are good to go.

## FAQ

### I make a change in PostgresConnection but the changes are not picked up

Do this:

- `cider-eval-buffer` postgres.clj
- `cider-eval-buffer` db.clj

Now you should be good to go.

### How to setup a test mysql db for testing?

```
docker run --rm --name=mysql-test-pine -p 3306:3306 -e MYSQL_ROOT_PASSWORD=secret -d mysql:5
```

Now, we can connect with the following configuration, using the password `secret`:

```
(setq sql-connection-alist
      '(
        (mysql-test
         (sql-product 'mysql)
         (sql-user "root")
         (sql-server "localhost")
         (sql-database "information_schema")
         )
        ))
```


Stop the container:

```
docker stop mysql-test-pine
```

### How to run the http server while the repl is running?

I could make it work by explicitly running a jetty server but as soon as I did
that, **cider stopped showing me evaluated results in emacs**.

Anyway, here are the steps:

In project.clj:

```
   :repl {
          :plugins [[cider/cider-nrepl "0.26.0"]
                    [refactor-nrepl "2.5.1"] ]
          :dependencies [[nrepl/nrepl "0.8.3"]
                         ;; [ring/ring-jetty-adapter "1.2.1"] ;; <-- add this dependency
                         ]
          }
```

Now, the server can be run explicitly once the `repl` is running (using `./repl.sh`):

```
(ns pine.repl
  (:require
   [compojure.handler :as handler]
   [pine.handler :as h]
   [ring.adapter.jetty :as ring]))

(ring/run-jetty (handler/site h/app) {:port 33333})
```

<!-- # Issue with Mysql 8 -->

<!-- ## Client does not support authentication protocol -->
<!-- ``` -->
<!-- com.mysql.jdbc.exceptions.jdbc4.MySQLNonTransientConnectionException: Client does not support authentication protocol requested by server; consider upgrading MySQL client -->
<!-- ``` -->

<!-- Naive fix: -->

<!-- ``` -->
<!-- ALTER USER 'root'@'localhost' IDENTIFIED WITH mysql_native_password BY 'secret'; -->
<!-- FLUSH PRIVILEGES; -->

<!-- ``` -->
