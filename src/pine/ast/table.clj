(ns pine.ast.table
  (:require
   [pine.db.main :as db]
   [pine.hints :as hints]))

(defn- join-helper [references t1 t2 a1 a2]
  "Find the references between the tables, get the columns for the first
  refernece and return the pair of alias and columns that will be used for the
  join"
  (when-let [reference (get-in references [:table t1 :refers-to t2 :via])]
    (let [c (-> reference keys first)            ;; This is a map indexed by columns. Get the first (and only) column
          join (-> (get-in reference [c]) first) ;; There can theoretically be multiple relations - we get the first as `c` is not specified
          [schema table col _ f-schema f-table f-col] join]
      [a1 col := a2 f-col])))

;; TODO: use spec for the state value i.e. first arg
(defn- join [{:keys [references aliases]} x y]
  (let [a1 (x :alias)
        a2 (y :alias)
        {t1 :table s1 :schema} (aliases a1)
        {t2 :table s2 :schema} (aliases a2)
        result (or (join-helper references t1 t2 a1 a2)
                   (join-helper references t2 t1 a2 a1))]
    result))

(defn- update-joins [state]
  "This function is called once the state is updated with the value of the new
  table - which is why we don't need the value to be passed in."
  (let [{:keys [tables joins]} state]
    (cond
      (< (count tables) 2) state
      :else
      (let [[x y] (take-last 2 tables)
            join-result (join state x y)]
        (assoc-in state [:joins (x :alias) (y :alias)] join-result)))))

(defn- update-hints [state token]
  (let [hints (hints/table-hints state token)]
    (assoc-in state [:hints :table] hints)))

;; todo: spec for the :value for a :table
(defn handle [state value]
  (let [{:keys [table alias schema]} value
        a (or alias (str table "_" (state :table-count)))]
    (-> state
        (update :tables conj {:table table :alias a})
        (update :aliases assoc a {:table table :schema schema})
        (update-joins)
        (update-hints table) ;; table is the token here
        ;;
        ;; TODO: These are metadata - mayebe I should move them to a different
        ;; ns e.g. if the operation is table, then I update the following there
        ;;
        (assoc  :context a)
        (update :table-count inc))))

