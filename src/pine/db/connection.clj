(ns pine.db.connection)

(defprotocol Connection
  "Abstractions for db engines e.g. mysql, postgres, etc"

  (get-connection-id [this] "Unique identified for the connection connection")

  ;; TODO: remove this in favor of references
  (get-tables [this] "Get tables.")

  (references [this table] "Get the tables used in the foreign keys")

  (get-columns [this table-name] "Get columns")


  (quote [this x] [this x y] "Quote table names, columns names, etc.")

  (quote-string [this x] "Quote values")

  (query [this statement] "Execute select statements"
    ;; where statement is e.g.
    ;; ["select * from public.user where id = ? limit 1" [:string "6a59c193-d480-41cc-94a8-aa7c3ba7ce54"]]
    )

  (execute! [this statement] "Execute non-select statements")

  ;; For debugging / internal use

  (get-config [this] "Internal db config used by the connection")
  )


