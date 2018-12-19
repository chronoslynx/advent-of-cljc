(ns aoc.y2018.d02.chronoslynx
  (:require
   [cljs.reader :as reader]
   [aoc.utils :as u :refer [deftest]]
   [aoc.y2018.d02.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]]))

(defn get-wanted-freqs [line]
  (into #{} (vals
             (filter #(or (= 2 (val %))
                          (= 3 (val %)))
                     (frequencies line)))))

(defn solve-1 []
  ;; TODO
  ;; Want: the number of lines containing at least one
  ;; letter that is doubled
  ;; as well as the number of lines containing at least one
  ;; tripled letter
  (let [stream (map get-wanted-freqs
                    (map clojure.string/trim
                         (clojure.string/split-lines input)))
        dubs (count (filter identity (map #(contains? % 2) stream)))
        trips (count (filter identity (map #(contains? % 3) stream)))]
    (* dubs trips)))

(defn count-diffs [line1 line2]
  (reduce + (map (fn [[a b]] (if (= a b) 0 1)) (map vector line1 line2))))

(defn compare-to [line lines]
  [line (filter #(= 1 (nth % 1))
                (map vector lines (map #(count-diffs line %) lines)))])

(defn common-letters [line1 line2]
  (map #(nth % 0) (filter (fn [[a b]] (= a b)) (map vector line1 line2))))

(defn solve-2 []
  ;; TODO
  ;; This can be done in N^2 time using the obvious line x line comparison.
  ;; Since they can differ at any place in the string a trie is useless.
  ;;
  ;;
  (let [lines (map clojure.string/trim (clojure.string/split-lines input))
        correct (nth (take 1 (filter #(not (empty? (nth % 1))) (map #(compare-to % lines) lines))) 0)
        line (nth correct 0)
        [match count] (nth (nth correct 1) 0)
        ]
    (apply str (common-letters line match))
    ))

(deftest part-1
  (is (= answer-1 (solve-1))))

(deftest part-2
  (is (= answer-2 (solve-2))))
