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
;;                      |      reports_to (FK)  |----|
;;                      +-----------------------+
;;                          ^
;;                          |       +------------------------+
;;                          |       |       z.document       |
;;                          |       |                        |
;;                          |       |      id (PK)           |
;;                          |-------|     employee_id (FK)   |
;;                          +-------|     created_by  (FK)   |
;;                                  +------------------------+

(def references [["y"  "employee"      "company_id"    "x"  "company"  "id"]
                 ["z"  "document"      "employee_id"   "y"  "employee" "id"]
                 ["z"  "document"      "created_by"    "y"  "employee" "id"]

                 ;; self join
                 ["y"  "employee"      "reports_to"   "y"  "employee" "id"]])

