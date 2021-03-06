(ns aoc.y2018.d01.chronoslynx
  (:require
   [aoc.utils :as u :refer [deftest]]
   [aoc.y2018.d01.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]]))

(def lines (memoize #(map u/read-string (clojure.string/split-lines input))))

(defn solve-1 []
  (reduce + (lines)))

(defn find-dup [state stream seen]
  (let [[x & xs] stream
        nstate (+ state x)]
    (if (contains? seen nstate)
      nstate
      (recur nstate xs (conj seen nstate)))))

(defn solve-2 []
  (let [nums (lines)
        state 0
        seen (set '(0))]
    (find-dup state (cycle nums) seen)))

(deftest part-1
  (is (= answer-1 (solve-1))))

(deftest part-2
  (is (= answer-2 (solve-2))))
