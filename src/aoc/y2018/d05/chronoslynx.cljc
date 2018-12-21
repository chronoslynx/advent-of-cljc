(ns aoc.y2018.d05.chronoslynx
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d05.data :refer [input answer-1 answer-2]]
   [clojure.string :as s]
   [clojure.test :as t :refer [is testing]]))

(defn =insensitive [c1 c2]
  (= (s/lower-case c1) (s/lower-case c2)))

(defn react
  ([[c & rest]] (react [c] rest))
  ([acc [c & rest]]
   (if (nil? c) acc
       (let [prev (peek acc)]
         (if (nil? prev)
           (recur (conj acc c) rest)
           (if (and (not (= prev c)) (=insensitive prev c))
             (recur (pop acc) rest)
             (recur (conj acc c) rest)))))))

(defn solve-1 []
  ;; TODO: find adjacent pairs of letters that are either lower Upper or Upper lower
  ;;
  ;; Could use a reduce to store previous element + results so far
  ;; Trivial algorithm gives us O(n^2) complexity (lose 2 characters each time). Worst
  ;; case input is abcdefgGFEDCBA, etc. Need an O(n) iteration to find a pair, and possibly
  ;; O(n) iterations overall.
  ;; Trivial algo 1.1: identify and remove all existing pairs
  ;; Trivial algo 1.2: repeat until fixed point
  (let [inpt (s/trim input)]
    (count (react inpt))))

(defn rm-unit [unit acc [c & rest]]
  (if (nil? c)
    acc
    (let [prev (peek acc)]
      (if (nil? prev)
        (recur unit (conj acc c) rest)
        (if (=insensitive c (peek acc))
          (recur unit (pop acc) rest)
          (recur unit (conj acc c) rest))))))

(defn chr->int [c]
  #?(:clj (int c)
     :cljs (.charCodeAt c 0)))

(defn solve-2 []
  ;; TODO: for each a-z, find all pairs and remove them (case-insensitive). Then react, and return the length of the shortest reacted string
  (let [inpt (s/trim input)
        candidates (map char (range (chr->int \a) (+ 1 (chr->int \z))))]
    (apply min (map (comp u/count' react) (for [c candidates] (filter #(not (=insensitive c %)) inpt))))))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))

;;;; Scratch

(comment
  (t/run-tests)
)
