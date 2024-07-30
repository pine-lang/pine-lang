(ns pine.ast.table
  (:require
   [pine.db.main :as db]))

(defn- join-helper [references t1 t2 a1 a2 c direction]
  "Find the references between the tables, get the columns for the first
  refernece and return the pair of alias and columns that will be used for the join"
  (when-let [reference (get-in references [:table t1 :referred-by t2 :via])]
    (let [f (if c (fn [_] c) (fn [xs] (if xs (first xs) nil)))
          col (-> reference keys f)
          join (-> (get-in reference [col]) first) ;; There can theoretically be multiple relations - we get the first as `c` is not specified
          [schema table col r f-schema f-table f-col] join]
      (if (= direction :reverse)
        [a2 f-col :of a1 col]
        [a1 col :has a2 f-col]))))

;; TODO: use spec for the state value i.e. first arg
(defn- join [{:keys [references aliases]} x y c parent]
  (let [a1 (x :alias)
        a2 (y :alias)
        {t1 :table s1 :schema} (aliases a1)
        {t2 :table s2 :schema} (aliases a2)
        result (or
                ;; By default we narrow the results i.e.
                ;; We get the children first and if a resultis not found, only
                ;; then we look at parents
                (if (not parent) (join-helper references t1 t2 a1 a2 c nil) nil)
                (join-helper references t2 t1 a2 a1 c :reverse))]
    result))

(defn- update-joins [state current]
  "Use the tables in the state to create a join between the last 2 tables. The
  reason to get the tables from the state is that they have been assigned an
  alias. We only use the join column from the current value being processed."
  (let [{:keys [join-column parent]} current
        from-alias                   (state :context)
        {:keys [tables join-map]}       state]
    (cond
      (nil? from-alias) state
      :else (let [x (-> state :aliases (get from-alias))
                  join-result (join state x current join-column parent)]
              (-> state
                  (assoc-in [:join-map (x :alias) (current :alias)] join-result)
                  (update :joins conj [(x :alias) (current :alias) join-result]))))))

(defn make-alias [s]
  (let [words (if (not-empty s) (clojure.string/split s #"_") ["x"])
        initials (map #(subs % 0 1) words)]
    (apply str initials)))

;; todo: spec for the :value for a :table
(defn handle [state value]
  (let [{:keys [table alias schema parent join-column]} value
        a (or alias (str (make-alias table) "_" (state :table-count)))
        current {:schema schema :table table :alias a :parent parent :join-column join-column}]
    (-> state
        ;; pre
        (assoc  :context (state :current))
        (assoc  :current a)
        ;;
        (update :tables conj current)
        (update :aliases assoc a current)
        (update-joins current)
        ;; post
        ;;
        ;; TODO: These are metadata - mayebe I should move them to a different
        ;; ns e.g. if the operation is table, then I update the following there
        ;;
        (update :table-count inc))))

