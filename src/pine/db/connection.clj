(ns pine.db.connection)

(defprotocol Connection
  "Abstractions for db engines e.g. mysql, postgres, etc"

  (get-connection-id [this] "Unique identified for the connection connection")

  (get-schema [this] "Deprecated. The schema should not be exposed")

  (get-tables [this] "Get tables.")

  (get-columns [this table-name] "Get columns")

  (references [this table] "Get the tables used in the foreign keys")

  (quote [this x] "Quote table names, columns names, etc.")

  (quote-string [this x] "Quote values")

  (query [this statement] "Execute select statements")

  (execute! [this statement] "Execute non-select statements")
  )


