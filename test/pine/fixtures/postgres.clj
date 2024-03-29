(ns pine.fixtures.postgres)

;; Following tables exist:
;;
;;   `public`.`organization`
;;   `x`.`user`
;;   `y`.`document`
;;
;;     +----------------------+
;;     |  public.organization |
;;     |                      |
;;     |        id (PK)       |
;;     +----------------------+
;;              ^
;;              |       +--------------------+
;;              |       |     x.user         |
;;              |       |                    |
;;              |       |       id (PK)      |
;;              +-------|      org_id (FK)   |
;;                      +--------------------+
;;                          ^
;;                          |       +--------------------+
;;                          |       |    y.document      |
;;                          |       |                    |
;;                          |       |      id (PK)       |
;;                          +-------|     user_id (FK)   |
;;                                  +--------------------+

(def relations [["x"  "user"      "org_id"    "public"  "organization"  "id"]
                ["y"  "document"  "user_id"   "x"       "user"          "id"]])


;; TODO: the following is deprecated
(def schema {
             :tenant
             #:db{:columns ["id"
                            "title"],
                  :foreign-keys {}},

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
                  :foreign-keys {:attachment "attachment_id",
                         :tenant "tenant_id"}},

             :attachment
             #:db{:columns ["id"
                            "key"
                            "originalname"
                            "createdAt"
                            "bucket"],
                  :foreign-keys {}}}

  )
