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
;;              +-------|      company_id (FK)  |
;;                      +-----------------------+
;;                          ^
;;                          |       +------------------------+
;;                          |       |       z.document       |
;;                          |       |                        |
;;                          |       |      id (PK)           |
;;                          +-------|     employee_id (FK)   |
;;                                  +------------------------+

(def references [["y"  "employee"      "company_id"    "x"  "company"  "id"]
                 ["z"  "document"      "employee_id"   "y"  "employee" "id"]])

