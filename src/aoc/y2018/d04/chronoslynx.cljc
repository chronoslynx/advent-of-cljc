(ns aoc.y2018.d04.chronoslynx
  (:require
   [clojure.string :refer [ends-with? trim split-lines]]
   [aoc.utils :as u :refer [deftest]]
   [aoc.y2018.d04.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]])
  (:import
   [java.time LocalDateTime format.DateTimeFormatter]
   [java.time.temporal ChronoUnit])
  )

(def date-format (DateTimeFormatter/ofPattern
                  "yyyy-MM-dd HH:mm"))

(defn parse-line [line]
  ;; Three different lines:
  ;;Strategy 1: Find the guard that has the most minutes asleep.
  ;; What minute does that guard spend asleep the most?
  ;; [1518-03-05 00:59] wakes up
  ;; [1518-04-19 00:22] falls asleep
  ;; [1518-10-18 23:51] Guard #349 begins shift
  ;; Returns one of (:falls-asleep :wakes-up int)
  ;; TODO: get a proper datetime out of here for sorting
  (let [trimmed (trim line)
        [dstr] (rest (re-matches
                      #"\[(\d{4}-\d\d-\d\d \d\d:\d\d)\].*"
                      trimmed))
        dt {:time (LocalDateTime/parse dstr date-format)}]
    (cond
      (ends-with? trimmed "ep") (assoc dt :act :falls-asleep)
      (ends-with? trimmed "up") (assoc dt :act :wakes-up)
      (ends-with? trimmed "ft") (-> dt
                                    (assoc :act :change-shift)
                                    (assoc :guard (Integer/parseInt (nth (re-matches #".*?Guard #(\d+) .*" trimmed) 1))))
      :else nil)))

(defn inc-or-set [hmap key]
  (update hmap key #(if (nil? %) 1 (inc %))))

(defn handle-event [state event]
  (let [{:keys [time act]} event]
    (case act
      :falls-asleep (assoc state :sleep-start (.toLocalTime time))
      ;; TODO - update the sleep times for the current guard
      :wakes-up (let [{:keys [sleep-start active-guard]} state
                      end (.toLocalTime time)
                      minutes (.until sleep-start end ChronoUnit/MINUTES)
                      times (map #(.plusMinutes sleep-start %) (range minutes))]
                  (update-in state [:sleep (keyword (str active-guard))]
                             (fn [guard]
                               (reduce #(inc-or-set %1 %2) guard times))))
      :change-shift (let [guard (:guard event)]
                      (assoc state :active-guard guard)))))

(def parsed (memoize #(filter some? (map parse-line (split-lines input)))))
(def sorted-entries (memoize #(sort-by :time (parsed))))
(def processed (memoize #(reduce handle-event {:sleep {}} (sorted-entries))))
(def top-minutes (memoize #(reduce-kv (fn [m k v] (assoc m k (apply max-key val v))) {} (:sleep (processed)))))


(defn solve-1 []
  (let [state (processed)
        sleep-times (:sleep state)
        sleep-totals (reduce-kv #(assoc % %2 (reduce + (vals %3))) {} sleep-times)
        [sleepiest-guard _] (apply max-key val sleep-totals)
        [sleepiest-minute _] (sleepiest-guard (top-minutes))
        ]
    (* (read-string (name sleepiest-guard)) (.getMinute sleepiest-minute))))

(defn solve-2 []
  (let [state (processed)
        sleep-times (:sleep state)
        minutes (top-minutes)
        [guard bestmin count] (reduce-kv
                               #(let [[minute count] %3
                                      [cguard cmin ccount] %]
                                  (if (> count ccount)
                                    [%2 minute count]
                                    %)) [0 0 0] minutes)]
    (* (read-string (name guard)) (.getMinute bestmin)))
  )

(deftest part-1
  (is (= answer-1 (solve-1))))

(deftest part-2
  (is (= answer-2 (solve-2))))
