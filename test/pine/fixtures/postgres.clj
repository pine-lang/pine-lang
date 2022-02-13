(ns pine.fixtures.postgres)

(def schema {:user
             #:db{:columns
                  ["id"
                   "email"
                   "status"
                   "password"
                   "salt"
                   "attachment_id"
                   "user_type"
                   "sessionId"
                   "tenant_id"],
                  :refs {:attachment "attachment_id", :tenant "tenant_id"}},
             :attachment
             #:db{:columns ["id" "key" "originalname" "createdAt" "bucket"],
                  :refs {}}
             })
