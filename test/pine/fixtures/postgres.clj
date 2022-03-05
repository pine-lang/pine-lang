(ns pine.fixtures.postgres)

(def schema {
             :tenant
             #:db{:columns ["id"
                            "title"],
                  :refs {}},

             :user
             #:db{:columns ["id"
                            "email"
                            "status"
                            "password"
                            "salt"
                            "attachment_id" ;; TODO: user should own attachment
                                            ;; not the other way around
                            "user_type"
                            "sessionId"
                            "tenant_id"],
                  :refs {:attachment "attachment_id",
                         :tenant "tenant_id"}},

             :attachment
             #:db{:columns ["id"
                            "key"
                            "originalname"
                            "createdAt"
                            "bucket"],
                  :refs {}}}

  )
