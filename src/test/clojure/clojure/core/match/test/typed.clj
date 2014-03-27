(ns clojure.core.match.test.typed
  (:use [clojure.test])
  (:require [clojure.core.match.typed :as mt]
            [clojure.core.typed :as t]))

(deftest check-match-test
  (is (do (t/cf (clojure.core.match.typed/match
                  [1 2]
                  [1 2] 1
                  :else 2)
                Number)
          true))
  (is (do (t/cf
            (let [x true
                  y true
                  z true]
              (clojure.core.match.typed/match 
                [x y z]
                [_ false true] 1
                [false true _ ] 2
                [_ _ false] 3
                [_ _ true] 4
                :else 5)))
          true))
  (is (do (t/cf ((fn [x y z done]
                   (if (not done)
                     (clojure.core.match.typed/match 
                       [x y z]
                            [_ false true] (recur x y z 1)
                            [false true _ ] (recur x y z 2)
                            [_ _ false] (recur x y z 3)
                            [_ _ true] (recur x y z 4)
                            :else 5)
                     done)) true true true false)
                [Boolean Boolean Boolean Boolean -> Boolean])
          true))
  )
