# Test Locally

## Use a test Mysql server

```
docker run --rm --name dummy -e MYSQL_ROOT_PASSWORD=dummy -p3306:3306 -d mysql:5
```

## Dummy configuration

Update the `config.sample` with the values:


```
    [db-host "host.docker.internal"
     db-port 3306
     db-name "information_schema"
     db-user "root"
     db-password "dummy"]
```

## Run the pine server

```
UID=${UID}  \
GID=${UID}  \
USR=${USER} \
 docker-compose up
```

or see `DEV.md`

<!-- # Issues -->

<!-- ## Client does not support authentication protocol -->
<!-- ``` -->
<!-- com.mysql.jdbc.exceptions.jdbc4.MySQLNonTransientConnectionException: Client does not support authentication protocol requested by server; consider upgrading MySQL client -->
<!-- ``` -->

<!-- Naive fix: -->

<!-- ``` -->
<!-- ALTER USER 'root'@'localhost' IDENTIFIED WITH mysql_native_password BY 'secret'; -->
<!-- FLUSH PRIVILEGES; -->

<!-- ``` -->

