(ns aoc.y2018.d03.chronoslynx
  (:require
   [aoc.utils :as u :refer [deftest]]
   [aoc.y2018.d03.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]]))


(defn parse-line
  "Parse a line like '#1 @ 1,3: 4x4' into a vector of
  [match requestNum x y width height] or nil"
  [line]
  ;;#1 @ 1,3: 4x4 -> num @ x,y: wxh
  (let [[i x y w h] (map u/read-string (rest (re-matches #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" (clojure.string/trim line))))]
    (for [nx (range x (+ x w))
          ny (range y (+ y h))]
      {:id i :coord [nx ny]})
    )
  )

(def parsed (memoize #(mapcat parse-line (clojure.string/split-lines input))))
(def freqs (memoize #(frequencies (map :coord (parsed)))))

(defn solve-1 []
  ;; TODO
  ;; make a vector the size of the board that contains ints
  ;; for each tile, add 1 to each point that the tile covers
  ;; Count the number of locations where the value is > 1
  ;; There are two basic ways to implement this: as a dense vector
  ;; (one location per x,y pair) or a map of coordinate -> value
  ;; Let's go for the second one
  ;;
  ;;  Even easier: use frequencies
  (count (filter (fn [[_ v]] (>= v 2)) (freqs))))

(defn has-overlap
  [{id :id coord :coord} frqs]
  (if (> (frqs coord) 1)
    {:id id :overlap true}
    {:id id :overlap false}))

(defn solve-2 []
  (let [fs (freqs)
        grouped (group-by :id (map #(has-overlap % fs) (parsed)))
        nonoverlapping (filter (fn [[id vs]] (not-any? :overlap vs)) grouped)]
    (str (nth (map #(nth % 0) nonoverlapping) 0)) ))

(deftest part-1
  (is (= answer-1 (solve-1))))

(deftest part-2
  (is (= answer-2 (solve-2))))
