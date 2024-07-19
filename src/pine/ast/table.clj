(ns pine.ast.table
  (:require
   [pine.db.main :as db]
   [pine.hints :as hints]))

(defn- join-helper [references t1 t2 a1 a2 c]
  "Find the references between the tables, get the columns for the first
  refernece and return the pair of alias and columns that will be used for the
  join"
  (when-let [reference (get-in references [:table t1 :refers-to t2 :via])]
    (let [fn (if c (fn [_] c) (fn [xs] (if xs (first xs) nil)))
          col (-> reference keys fn)
          join (-> (get-in reference [col]) first) ;; There can theoretically be multiple relations - we get the first as `c` is not specified
          [schema table col _ f-schema f-table f-col] join]
      [a1 col := a2 f-col])))

;; TODO: use spec for the state value i.e. first arg
(defn- join [{:keys [references aliases]} x y c]
  (let [a1 (x :alias)
        a2 (y :alias)
        {t1 :table s1 :schema} (aliases a1)
        {t2 :table s2 :schema} (aliases a2)
        result (or (join-helper references t1 t2 a1 a2 c)
                   (join-helper references t2 t1 a2 a1 c))]
    result))

(defn- update-joins [state {:keys [join-column]}]
  "Use the tables in the state to create a join between the last 2 tables. The
  reason to get the tables from the state is that they have been assigned an
  alias. We only use the join column from the current value being processed."
  (let [{:keys [tables joins]} state]
    (cond
      (< (count tables) 2) state
      :else
      (let [[x y] (take-last 2 tables)
            join-result (join state x y join-column)]
        (assoc-in state [:joins (x :alias) (y :alias)] join-result)))))

(defn- update-hints [state value]
  (let [hints (if (= (state :pending-count) 1)
                (hints/generate state value)
                {:table []})]
    (assoc-in state [:hints] hints)))

(defn make-alias [s]
  (let [words (if (not-empty s) (clojure.string/split s #"_") ["x"])
        initials (map #(subs % 0 1) words)]
    (apply str initials)))

;; todo: spec for the :value for a :table
(defn handle [state value]
  (let [{:keys [table alias schema]} value
        a (or alias (str (make-alias table) "_" (state :table-count)))]
    (-> state
        (update :tables conj {:schema schema :table table :alias a})
        (update :aliases assoc a {:table table :schema schema})
        (update-joins value)
        (update-hints value)
        ;;
        ;; TODO: These are metadata - mayebe I should move them to a different
        ;; ns e.g. if the operation is table, then I update the following there
        ;;
        (assoc  :context a)
        (update :table-count inc))))

