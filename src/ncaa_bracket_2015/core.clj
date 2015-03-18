(ns ncaa-bracket-2015.core
  (:refer-clojure :exclude [atom doseq let fn defn ref dotimes defprotocol loop for])
  (:require [clojure.core.typed :refer :all])
  (:require [clojure.data.csv :as csv])
  (:require [clojure.java.io :as io])
  (:gen-class))

(def final-four
  '(("Midwest" "West")
    ("East" "South")))

(def region-layout
  '((((6 11)
      (3 14))
     ((7 10)
      (2 15)))
    (((1 16)
      (8 9))
     ((5 12)
      (4 13)))))

(defn- read-teams-and-seeds []
  (with-open [in-file (io/reader "data/bracket-00.tsv")]
    (let [records (seq (doall (csv/read-csv in-file :separator \tab)))]
      (map
       (fn [record]
         (let [[_ name seed region kenpom] record]
           ; TODO parse seed into numeric and play-in status
           {:name name :seed (read-string (re-find #"\d+" seed)) :region region :kenpom (read-string kenpom)}
           ))
       (rest records)))))

(def teams-and-seeds
  (read-teams-and-seeds))

(defn- fill-using-layout [base-fn layout]
  (map
   (if (seq? (first layout))
     (partial fill-using-layout base-fn) ;; recurse
     base-fn ;; base case
     ) layout))

(defn- region-bracket [region layout]
  (fill-using-layout
   (fn [seed]
     (let [seeds
           (filter (fn [team]
                     (and (= seed (:seed team))
                          (= region (:region team))))
                   teams-and-seeds)]

       ;; Handle "first-four" games where there are multiple teams w/ same seed
       (if (= 1 (count seeds))
         (first seeds)
         seeds)))
   layout))

(defn- bracket [layout]
  (fill-using-layout
   (fn [region] (region-bracket region region-layout))
   layout))

(def full-bracket
  (bracket final-four))

(ann pvictory [Number Number -> Number])
(defn- pvictory [pythag-winner pythag-loser]
  (let [a pythag-winner
        b pythag-loser]
    (/ (- a (* a b))
       (+ a b (* -2 a b)))))

;; Finds the victor and their victory probability assuming favorites win all matches
(defn- pick-result [match]
  (let [probs (map :kenpom match)
        p-first-is-victor (apply pvictory probs)
        victor (if (> p-first-is-victor 0.5) (first match) (last match))
        p-victor (if (> p-first-is-victor 0.5) p-first-is-victor (- 1 p-first-is-victor))
        cumulative-victor-probability (:cumulative-probability victor)]

    ;; Prepend this victory probability to the existing sequence
    (assoc victor :cumulative-probability
           (cons
            (* (first cumulative-victor-probability) p-victor)
            cumulative-victor-probability))))

(defn- find-most-likely-victor [sub-bracket]
  (if (seq? sub-bracket)
    (let [sub-bracket-results (map find-most-likely-victor sub-bracket)]
      (pick-result sub-bracket-results))
    (let [team sub-bracket]
      (assoc team :cumulative-probability [1.0]))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (pvictory 0.8 0.7)))
