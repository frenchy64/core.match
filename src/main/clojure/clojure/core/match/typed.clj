(ns clojure.core.match.typed
  "This namespace provides match macros friendly with core.typed."
  (:require [clojure.core.match :as m]
            [clojure.core.match.debug :as dbg]
            [clojure.core.match.protocols :as prtcl]
            [clojure.core.typed :as t]))

(defn emit-flat 
  [{:keys [rows ocrs] :as m}]
  `(cond
     ~@(mapcat
         (fn [row]
           [`(and ~@(map (fn [p o] 
                           (cond
                             (m/wildcard-pattern? p) true
                             :else (prtcl/to-source* p o)))
                         row ocrs))
            (:action row)])
         rows)))

(defn flat-clj-form 
  "Translate an unoptimised pattern matrix directly into a cond"
  [vars clauses]
  (when @m/*syntax-check* (m/check-matrix-args vars clauses))
  (-> (m/emit-matrix vars clauses)
      emit-flat))

(defmacro match 
  "Pattern match a row of occurrences. Take a vector of occurrences, vars.
  Clause question-answer syntax is like `cond`. Questions must be
  wrapped in a vector, with same arity as vars. Last question can be :else,
  which expands to a row of wildcards.
  
  Example:
  (let [x 1
        y 2]
    (match [x y 3]
      [1 2 3] :answer1
      :else :default-answer))"
  [vars & clauses]
  (let [[vars clauses]
        (if (vector? vars)
          [vars clauses]
          [(vector vars)
            (mapcat (fn [[c a]]
                      [(if (not= c :else) (vector c) c) a])
              (partition 2 clauses))])]
   (binding [m/*line* (-> &form meta :line)
             m/*locals* (dissoc &env '_)
             m/*warned* (atom false)]
     (if true #_t/*currently-checking-clj*
       `~(flat-clj-form vars clauses)
       `~(m/clj-form vars clauses)))))


(comment
(dbg/build-matrix [1 2 3]
                  [1 2 3] :foo
                  [2 3 4] :foo1
                  :else 5)

(macroexpand
'(match [[1 2 nil nil nil]]
       [([1] :seq)]     :a0
       [([1 2] :seq)]   :a1
       [([1 2 nil nil nil] :seq)] :a2
       :else []))

(macroexpand
'(match [1 2 3]
       [1 2 3] :foo
       [2 3 4] :foo1
       :else 5))
  )
