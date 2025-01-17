(ns pine.db.fixtures)

;; Following tables exist:
;;
;;   `x`.`company`
;;   `y`.`employee`
;;   `z`.`document`
;;
;;     +------------------+
;;     |    x.company     |
;;     |                  |
;;     |     id (PK)      |
;;     +------------------+
;;              ^
;;              |       +-----------------------+
;;              |       |     y.employee        |
;;              |       |                       |
;;              |       |       id (PK)         |
;;              +-------|      company_id (FK)  |<---|
;;              |       |      reports_to (FK)  |----|
;;              |       +-----------------------+
;;              |           ^
;;              |           |       +------------------------+
;;              |           |       |       z.document       |
;;              |           |       |                        |
;;              |           |       |      id (PK)           |
;;              |           |-------|     employee_id (FK)   |
;;              |           +-------|     created_by  (FK)   |
;;              +-------------------|     company_id (FK)    |
;;                                  +------------------------+

(def foreign-keys [["y"  "employee"      "company_id"    "x"  "company"  "id"]
                   ["z"  "document"      "employee_id"   "y"  "employee" "id"]
                   ["z"  "document"      "created_by"    "y"  "employee" "id"]

                   ;; self join
                   ["y"  "employee"      "reports_to"    "y"  "employee" "id"]

                   ["z"  "document"      "company_id"    "x"  "company" "id"]])

;; schema table col pos type len nullable default
(def columns [["x"  "company"   "id"           nil  nil  nil  nil  nil]
              ["y"  "employee"  "id"           nil  nil  nil  nil  nil]
              ["y"  "employee"  "company_id"   nil  nil  nil  nil  nil]
              ["y"  "employee"  "reports_to"   nil  nil  nil  nil  nil]
              ["z"  "document"  "id"           nil  nil  nil  nil  nil]
              ["z"  "document"  "employee_id"  nil  nil  nil  nil  nil]
              ["z"  "document"  "created_by"   nil  nil  nil  nil  nil]
              ["z"  "document"  "company_id"   nil  nil  nil  nil  nil]])

(def references [foreign-keys columns])
