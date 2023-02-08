# rules
clojure/script rule engine

```
beta quality
```
## Install
```clojure
;; in deps.edn
{:deps {github-akovantsev/rules
        {:git/url "https://github.com/akovantsev/rules"
         :sha     "87108b4fa2a0caceea1a59c284ac3baf38108819"}}} ;; actual sha
```


## Usage example
https://github.com/akovantsev/rules/blob/main/test/com/akovantsev/rules/test.clj#L56

## Rule

```clojure
(def autoinc
  (r/parse-rule                  ;; macro
    [::foo                       ;; rule-id
     [:new id :x x < 5]          ;; find all ids whos :x is < 5
     [:let id :y y]              ;; join on id, where :y is not nil
     [:old id :x ox not= x]      ;; join on id, where prev :x val ≠ curr :x val

     :foreach                    ;; for each match {:id ... :x ... :y ...}:
     (prn :foreach1 match)       ;; print match map
     (r/change! id :x inc)       ;; (update current-db id :x inc)

     :forall                     ;; once, for matches seq ({:id ... :x ... :y ...} ...):
     (println :once1 matches)])) ;; print matches
```

## about

Similar to https://github.com/oakes/odoyle-rules with some differences:
- `odoyle` implements `rete`. `rules` does not.
- `odoyle` discards datoms wich don't have matching tuples in any rules.
`rules` does not, so you can store more things than you query,
and don't keep another `db` map on the side.
- `odoyle` allows to compare only(?) value bindings with only their own past value.
`rules` allows you to compare with any past value:
```clojure
; odoyle:
:what
[foo ::left-of bar {:then not=}]
[bar ::color color]

; rules:
[:new bar ::color color my-compare-with obar]
[:new foo ::left-of bar not= obar]
[:old foo ::left-of obar]
```

### LHS/query/tuples format
Tuples' order within rule does not matter. Tuples are sorted during rule parsing.
```clojure
[tuple-type e a v]
[tuple-type e a v f & args]
```
`tuple-type` is one of:
  - `:new` will use `current db` for bindings; any change in db's `a`(ttribute) will `trigger` rule.
  - `:old` will use `previous db` for bindings; does not `trigger` rule.
  - `:let` will use `current db` for bindnigs; does not `trigger` rule.

```clojure
; e.g when db changes from {1 {:a 1 :b 1}} to {1 {:a 2 :b 2}}
; change of any :a attribute in db does not trigger this rule,
; because it is not used in :new.
; only changes of :b in db trigger rule:
[:old id :a oa]              ;; :old uses :a, but :a change in db does not trigger rule
[:let id :a a not= (+ 1 oa)] ;; :let uses :a, but :a change in db does not trigger rule
[:new id :b b]               ;; :new uses :b, and any :b change in db triggers the rule
```

`triggers` - means `query/what/LHS/LeftHandSide` part of the rule is executed,
and if there are matches - `forall/foreach/then/then-finally/RHS/RightHandSide` part of the rule is executed:
- first `foreach` (if present) is executed with each match from `query` part (e.g. 10 patches => 10 `foreach` calls)
- then `forall` (if present) is executed once with all the matches (e.g. 100 matches => 1 `forall` call)

## Guards
Guards are (additional to joins) constraints on `v` bindings (not `e` ones).
<br>You can reference any other binding from rule in predicate.
<br>By default `e` binding is passed as first arg to `f`:
```clojure
;;[type e a v f & args]:

[:new id :a a < b]  ;; -> (fn [{:as match, :syms [a b]}] (< a b))
[:new id :b b odd?]
```
But you can specify place with `%`.
```clojure
[:new id :a a < b % (+ 10 id)]  ;; -> (fn [{:as match, :syms [a b id]}] (< b a (+ 10 id))
[:new id :b b]
```
(for now) it works only on 1st level though, so this is gonna blow up with `cannot resolve %`:
```clojure
[:new id :a a < b (inc %) 10]  ;; -> (fn [{:as match, :syms [a b]}] (< a b (inc %) 10))
[:new id :b b]
```

## Callbacks / RHS

There are 2 rule callbacks available.
<br>You can use both in the same rule.
<br>Both are optional. Rule without any of those means it is just a `query`.
<br>Order of appearance in rule definition does not matter.
<br><br>`:foreach` is called for every match of `query/LHS`, and sees:
- current `db` (same as `session` in `odoyle` and `clara`),
- all `matches` seq,
- current `match` map,
- all `e` and `v` bindings,
<br>Forms after `:foreach` until the `:forall` or end of rule are the body of callback: 
```clojure
...
[:new id :a a]
:foreach
(println db matches match id a)
(println id a)
;; => (fn [db matches {:as match, :syms [id a]}]
;;      (println db matches match)
;;      (println id a))
```

`:forall` is is called only once, after `:foreach` (if there is `:foreach` in the rule), and sees only:
- current `db`,
- all `matches` seq.
```clojure
:forall
(println db)
(println matches)
;; => (fn [db matches]
;;      (println db)
;;      (println matches))
```