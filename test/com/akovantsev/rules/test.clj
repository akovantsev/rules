(ns com.akovantsev.rules.test
  (:require [com.akovantsev.rules :as r]))


(r/insert {} 1 :x 2)
(r/insert {} 1 {:x 2 :y 3 :z 4})
(r/insert {} {1 {:x 2 :y 3 :z 4}
              2 {:x 2 :y 3 :z 4}})

(binding [r/*db* (volatile! {})]
  (r/insert! 1 2 3))


(r/rule ::rule-foo
   [:new id :x x > (- 1 z)]
   [:new id :y y > (or oy 2)]
   [:let id :z z]
   [:old id :y oy]
   :foreach
   (cond
     (< x 0) (println ['< 0 x y z])
     :else   (println [:else x y z])))



(r/by-id {:aev-new {:a {1 2 3 4 5 6}
                    :b {1 2 3 4 5 6 7 8}}}
  7)



(def autoinc
  (r/rule ::foo
     [:new id :x x < 5]
     [:let id :y y]
     :forall
     (println :once1 matches)
     :foreach
     (prn :foreach1 match)
     (r/change! id :x inc)))


(def autoinc2
  (r/rule ::foo2
     [:new id :x x <= 5 % 10]
     :forall
     (println :once2 matches)
     :foreach
     (prn :foreach2 match)
     (r/change! id :x + 2)))


(->  {}
  (r/init {:foo {:x 0} :bar {:x 3}})
  (r/add-rules [autoinc autoinc2])
  (r/fire-rules)
  (r/insert :foo :x 1)
  (r/fire-rules {::depth-limit 33})
  (r/insert :foo :x 2)
  (r/insert :bar :y 2)
  (r/fire-rules {::depth-limit 33})
  (r/change :bar :x - 5)
  (r/fire-rules)
  (r/by-id :bar)
  (time))


(set! *print-namespace-maps* false)
(declare db match matches)


(defn spy [x & [ks]]
  (if ks
    (clojure.pprint/pprint (select-keys x ks))
    (clojure.pprint/pprint x))
  x)

(r/rule ::foo [:new id :a a or % (+ 1 %)])

(def rules2
  [(r/rule ::light
     [:old :event :switch false]
     [:new :event :switch true]
     [:let :state :light nc]
     [:old :state :light oc #{"red" "green" "yellow"}]
     :foreach
     (let [color (case nc
                   "green"  "yellow"
                   "red"    "yellow"
                   "yellow" (case oc
                              "red" "green"
                              "green" "red"))]
       ;(prn ::light match color)
       (r/insert! {:event {:switch false}
                   :state {:light color}})))])
       ;(spy (select-keys db [:aev-old :aev-new]))
       ;(spy (select-keys @r/*db* [:aev-old :aev-new]))))])


(-> {}
  (r/add-rules rules2)
  (r/init {:event {:switch false}
           :state {:light "red"}}
    {::r/cb-before-rule-exec  (fn [m] (prn ::r/cb-before-rule-exec (keys m)))
     ::r/cb-after-guard-match (fn [m] (prn ::r/cb-after-guard-match (keys m)))
     ::r/cb-after-tuple-match (fn [m] (prn ::r/cb-after-tuple-match (keys m)))})
  ;(r/fire-rules)
  (r/insert :event :switch true)
  (r/fire-rules)
  ;(spy [:aev-old :aev-new])
  (r/insert :event :switch true)
  (r/fire-rules)
  ;(spy [:aev-old :aev-new])
  (r/insert :event :switch true)
  (r/insert :event :switch false)
  (r/insert :event :switch true)
  (r/fire-rules)
  ;(spy [:aev-old :aev-new])
  (r/insert :event :switch true)
  (r/fire-rules)
  (select-keys [:aev-old :aev-new]))

