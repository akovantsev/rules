(ns com.akovantsev.rules
  #?(:cljs (:require-macros [com.akovantsev.rules :refer [rule]]))
  (:require [clojure.walk :as walk]))

(def -conjs (fnil conj #{}))
(def -conjv (fnil conj []))
(def -plus  (fnil + 0))


(defn -prewalk-replace [smap form] ;; for cljs, copy of from clojure.walk/prewalk-replace
  (walk/prewalk (fn [x] (if (contains? smap x) (smap x) x)) form))

(defn -parse-tuple [locals [t e a v f & args :as raw]]
  (let [deps (->> args (tree-seq coll? seq) (filter locals) set)
        vars (->> [e v] (filter symbol?) set)]
    (merge {:t    t
            :e    (list 'quote e)
            :a    a
            :v    (list 'quote v)
            :vars (list 'quote vars)
            :deps (list 'quote deps)
            :raw  (list 'quote raw)}
      (when f
        (let [gargs (filter locals (conj deps v))
              call  (if (some #{'%} args)
                      `(~f ~@(-prewalk-replace {'% v} args))
                      `(~f ~v ~@args))
              guard (if (seq gargs)
                      `(fn [{:syms [~@gargs] :as ~'match}] ~call)
                      `(fn [~'match] ~call))]
          {:guard-fn   guard
           :guard-form (list 'quote guard)}))
      (case [(symbol? e) (symbol? v)]
        [false false] {:kind :eav :sort 1} ;; constant check, no need for var queries if this fails
        [false true]  {:kind :ea? :sort 2} ;; fixed e means only 1 or 0 possible ?v of a
        [true false]  {:kind :?av :sort 3} ;; fixed v means all or fewer possible ?e with a
        [true true]   {:kind :?a? :sort 4})))) ;;means all possible ?e with a


#?(:clj
   (defmacro rule [rule-id & rule-body]
     (let [[tuplevs & pairs] (partition-by #{:foreach :forall} rule-body)
           mpairs    (apply hash-map pairs)
           ;_ (prn mpairs)
           foreach   (get mpairs '(:foreach))
           forall    (get mpairs '(:forall))
           rulem     (reduce
                       (fn [m [t e a v]]
                         (cond-> m
                           (symbol? e) (update :locals update e -plus 1)
                           (symbol? v) (update :locals update v -plus 1)
                           (= t :new)  (update :watch-attrs conj a)))
                       {:rule-id     rule-id
                        :locals      {}
                        :watch-attrs #{}}
                       tuplevs)
           locals    (:locals rulem)
           tuplems   (->> tuplevs
                       (map #(-parse-tuple locals %))
                       (sort-by :sort <)
                       vec)
           then-fn   (when foreach
                       `(fn [~'db ~'matches {:syms [~@(keys locals)] :as ~'match}] ~@foreach))
           once-fn   (when forall
                       `(fn [~'db ~'matches] ~@forall))]
       (-> rulem
         (update :locals #(list 'quote %))
         (assoc :then-fn then-fn)
         (assoc :once-fn once-fn)
         (assoc :then-form (when then-fn (list 'quote then-fn)))
         (assoc :once-form (when then-fn (list 'quote once-fn)))
         (assoc :tuples tuplems)))))




(defn -insert [db m old?]
  (let [rf (fn [db id kvs]
             (let [{:keys [:a->rules :aev-new :temp]} db]
               (reduce-kv
                 (fn [m k v]
                   ;(prn 'assoc k v)
                   (let [tri? (seq (get a->rules k))
                         pair [id k]
                         acc? (and (not old?) (not (contains? temp pair)))]
                     (-> m
                       (assoc-in [:aev-new k id] v)
                       (cond->
                         old? (assoc-in [:aev-old k id] (-> aev-new (get k) (get id)))
                         acc? (assoc-in [:aev-old k id] (-> aev-new (get k) (get id)))
                         acc? (update :temp -conjs pair)
                         tri? (update :triggered -conjs k)))))
                 db kvs)))]
    (reduce-kv rf db m)))


(defn -change
  ([db id-k-f old?] (reduce-kv #(-change %1 %2 %3 old?) db id-k-f))
  ([db id k-f old?] (reduce-kv #(-change %1 id %2 %3 nil old?) db k-f))
  ([db id k f args old?]
   (let [{:keys [:a->rules :aev-new :temp]} db
         pair [id k]
         acc? (and (not old?) (not (contains? temp pair)))
         tri? (seq (get a->rules k))]
     (as-> db db
       (apply update-in db [:aev-new k id] f args)
       (cond-> db
         old? (assoc-in [:aev-old k id] (-> aev-new (get k) (get id)))
         acc? (assoc-in [:aev-old k id] (-> aev-new (get k) (get id)))
         acc? (update :temp -conjs pair)
         tri? (update :triggered -conjs k))))))


(defn insert
  ([db id k v] (insert db id {k v}))
  ([db id k-v] (insert db {id k-v}))
  ([db id-k-v] (-insert db id-k-v false)))

(defn change
  ([db id-k-f] (-change db id-k-f false))
  ([db id k-f] (-change db id k-f false))
  ([db id k f & args] (-change db id k f args false)))

(defn init
  ([db m] (init db m nil))
  ([db m callbacks]
   (as-> db db
     (insert db m)
     (assoc db :aev-old (:aev-new db))
     (assoc db ::callbacks callbacks))))

(defn by-id [db id]
  (let [m (reduce-kv
            (fn [m a ev]
              (if-let [[e v] (find ev id)]
                (assoc m a v)
                m))
            nil
            (:aev-new db))]
    (some-> m (assoc ::id id))))


(def ^:dynamic *db* nil)

(defn insert!
  ([id k-v] (insert! {id k-v}))
  ([id k v] (insert! id {k v}))
  ([id-k-v] (if-not *db*
              (throw (ex-info "can call insert! only within rule" {'arg id-k-v}))
              (vswap! *db* -insert id-k-v true))))


(let [ex! (fn [& args] (throw (ex-info "can call change! only within rule" {'call (cons 'change! args)})))]
  (defn change!
    ([id-k-f]        (if *db* (vswap! *db* -change id-k-f true) (ex! id-k-f)))
    ([id k-f]        (if *db* (vswap! *db* -change id k-f true) (ex! id k-f)))
    ([id k f & args] (if *db* (vswap! *db* -change id k f args true) (ex! id k f args)))))



(defn -make-match-rf [aev-old aev-new]
  (fn match-rf [matches {:keys [t e a v kind]}]
    ;(prn [kind t e a v] matches)
    (let [aev (if (= t :old) aev-old aev-new)
          ev  (get aev a)]
      ;(prn aev)
      (if (empty? ev)
        (reduced [])
        (case kind
          :eav
          (let [[E V :as found] (find ev e)]
            (if (and found (= V v))
              [{}] ;; rule can have no bindings, but needs to be able to trigger :then
              (reduced [])))

          :ea?
          (let [[E V :as found] (find ev e)]
            (if-not found
              (reduced [])
              (let [[E0 V0 :as found] (-> matches first (find e))]
                (if-not found
                  (update matches 0 assoc v V)
                  (if (= V0 V)
                    matches
                    (reduced []))))))

          :?av
          (let [Es (->> ev
                     (keep (fn [[E V]] (when (= v V) E)))
                     set)]
            (cond
              (empty? Es)
              (reduced [])

              (nil? matches) ;; nil means processing first tuple.
              (map #(-> {e %}) Es)

              (-> matches first (contains? e))
              (->> matches (filter (fn [m] (contains? Es (get m e))))) ;; id can be nil

              :else
              (->> matches (mapcat (fn [m] (map #(assoc m e %) Es))))))

          :?a?
          (let [hase (-> matches first (contains? e))
                hasv (-> matches first (contains? v))]
            ;(prn :hase hase :hasv hasv)
            (if hase
              (if hasv
                (->> matches
                  (filter (fn [m]
                            ;(prn m ev)
                            (let [[E V :as found] (find ev (get m e))]
                              ;(prn :E :V E V)
                              (and found (= E (get m e)) (= V (get m v)))))))
                (->> matches
                  (keep (fn [m]
                          (when-let [[E V :as found] (find ev (get m e))]
                            (assoc m v V))))))
              (if hasv
                (->> matches
                  (mapcat (fn [m]
                            (for [[E V] ev :when (= V (get m v))]
                              (assoc m e E)))))
                (if (nil? matches)
                  (map (fn [[E V]] {e E v V}) ev)
                  (->> matches
                    (mapcat (fn [m]
                              (map (fn [[E V]] (assoc m e E v V)) ev)))))))))))))


(defn query [db rule-id & [callbacks]]
  (let [callbacks (-> db ::callbacks (merge callbacks))
        cb-brq    (::cb-before-rule-query callbacks)
        cb-atm    (::cb-after-tuple-match callbacks)
        cb-agm    (::cb-after-guard-match callbacks)
        rulem     (-> db :rules (get rule-id))
        _         (assert rulem (str "no such rule in db: " rule-id))
        tuples    (:tuples rulem)
        mrf       (-make-match-rf (:aev-old db) (:aev-new db))
        rf        (if-not cb-atm
                    mrf
                    (fn [ms t]
                      (let [ms* (mrf ms t)]
                        (when cb-atm (cb-atm {::db db ::rule rulem ::tuple t ::matches-before ms ::matches-after ms*}))
                        ms*)))
        mk-guard  (if-not cb-agm
                    (fn [t] (fn [m] (when ((:guard-fn t) m) m)))
                    (fn [t] (fn [m] (let [m* (when ((:guard-fn t) m) m)]
                                      (cb-agm {::db db ::rule rulem ::tuple t ::match-before m ::match-after m*})
                                      m*))))
        guards    (some->> tuples
                    (filter :guard-fn)
                    (map mk-guard)
                    seq
                    (apply every-pred))]
    (when cb-brq (cb-brq {::db db ::rule rulem}))
    (seq
      (cond->> (reduce rf nil tuples)
        guards (filter guards)))))


(defn replace-rule [db rulem]
  (let [{:keys [:rule-id :watch-attrs]} rulem]
    (as-> db db
      (update db :rules assoc rule-id rulem)
      (update db :rules-ord -conjv rule-id)
      (reduce
        (fn [m a] (update-in m [:a->rules a] -conjs rule-id))
        db
        watch-attrs))))

(defn add-rule [db rulem]
  (assert (not (contains? (:rules db) (:rule-id rulem)))
    (str "rule " (:rule-id rulem) " rule is already registered. Use replace-rule instead."))
  (replace-rule db rulem))

(defn add-rules [db rulems] (reduce add-rule db rulems))
(defn replace-rules [db rulems] (reduce replace-rule db rulems))

(defn -ord-triged-rules-ids [db]
  (let [triggered-ids (reduce into #{} (vals (select-keys (:a->rules db) (:triggered db))))]
    (filter triggered-ids (:rules-ord db))))


(defn -exec-rule [db rule-id]
  (let [rule   (-> db :rules (get rule-id))
        cbs    (::callbacks db)
        cb-ber (::cb-before-rule-exec cbs)
        onefn  (:then-fn rule)
        allfn  (:once-fn rule)]
    (when cb-ber (cb-ber {::db db ::rule rule}))
    (if-not (or onefn allfn)
      db ;; dont exec just-queries
      (let [matches (query db rule-id)]
        (if (empty? matches)
          db
          (binding [*db* (volatile! db)]
            (when onefn
              (doseq [match matches]
                (onefn db matches match)))
            (when allfn
              (allfn db matches))
            (update-in @*db* [:calls rule-id] -plus 1)))))))


(defn fire-rules [db & [opts]]
  (let [{:keys [::callbacks
                ::depth-limit
                ::only-rule-ids
                ::ignore-rule-ids]} opts
        limit          (or depth-limit 10)
        orig-callbacks (::callbacks db)
        db*            (update db ::callbacks merge callbacks)
        callbacks*     (::callbacks db*)
        cb-bfr         (::cb-before-fire-rules callbacks*)
        cb-afr         (::cb-after-fire-rules callbacks*)
        triggered-ids  (-ord-triged-rules-ids db)]
    ; old ≠ new at this point
    (when cb-bfr (cb-bfr {::db db ::triggered-rule-ids triggered-ids}))
    (loop [db    db*
           ids   triggered-ids]
      (cond
        (and (not= limit ##Inf) (<= limit (reduce max 0 (vals (:calls db)))))
        (throw (ex-info "recur limit hit" {'db db}))

        (empty? ids)
        (do
          (when cb-afr (cb-afr {::db db}))
          (-> db
            (assoc ::callbacks orig-callbacks)
            (dissoc :calls :temp)))

        :else
        (let [id     (first ids)
              ids-   (rest ids)
              db+    (-> db
                       (dissoc :triggered)
                       (-exec-rule id))
              ids+   (->> ids-
                       (concat (-ord-triged-rules-ids db+))
                       (distinct))]
          (recur db+ ids+))))))
