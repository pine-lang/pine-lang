(ns pine.db.protocol)

(defprotocol DbAdapter
  "Abstractions for db engines e.g. mysql, postgres, etc"
  (connection [this] "Get the connection")
  (get-schema [this config]

  "Get the schema for the database. This function gets the schema for every table
  and can be very slow. Should be called once and the schema should be passed
  around."
    )
  (get-columns [this schema table-name] "Get columns")
  (references [this schema table] "Get the tables used in the foreign keys")
  (quote [this x] "Quote table names, columns names, etc.")
  (quote-string [this x] "Quote values")
  )


