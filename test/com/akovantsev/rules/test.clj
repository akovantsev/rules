(ns com.akovantsev.rules.test
  (:require [com.akovantsev.rules :as r]))


(r/insert {} 1 :x 2)
(r/insert {} 1 {:x 2 :y 3 :z 4})
(r/insert {} {1 {:x 2 :y 3 :z 4}
              2 {:x 2 :y 3 :z 4}})

(binding [r/*db* (volatile! {})]
  (r/insert! 1 2 3))


(r/parse-rule
  [::rule-foo
   [:new id :x x > (- 1 z)]
   [:new id :y y > (or oy 2)]
   [:let id :z z]
   [:old id :y oy]
   :foreach
   (cond
     (< x 0) (println ['< 0 x y z])
     :else   (println [:else x y z]))])



(r/by-id {:aev-new {:a {1 2 3 4 5 6}
                    :b {1 2 3 4 5 6 7 8}}}
  7)



(def autoinc
  (r/parse-rule
    [::foo
     [:new id :x x < 5]
     [:let id :y y]
     :forall
     (println :once1 matches)
     :foreach
     (prn :foreach1 match)
     (r/change! id :x inc)]))


(def autoinc2
  (r/parse-rule
    [::foo2
     [:new id :x x <= 5 % 10]
     :forall
     (println :once2 matches)
     :foreach
     (prn :foreach2 match)
     (r/change! id :x + 2)]))


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
