(ns ncaa-bracket-2015.core
  (:refer-clojure :exclude [atom doseq let fn defn ref dotimes defprotocol loop for])

  (:require [clojure.core.typed :refer :all]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :refer [cartesian-product permutations]])

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


(defn- play-reach-distributions
  "Take a pair of reach-distributions and compute the most likely outcomes of a game between them"
  [limit reach-distributions]
  (let [distributions (map :reach-distribution reach-distributions)
        ;; Pair off all the teams
        pairings (apply cartesian-product distributions)
        ;; Blow up into a list with both sets of outcomes for a pair (team A wins, team B wins)
        outcomes (mapcat permutations pairings)
        ;; Find the probability of each outcome based on the probability of the team reaching the round and their relative pyth
        outcome-probabilities (map
                               (fn [outcome]
                                 (let [teams (map :team outcome)
                                       win-probability (apply pvictory (map :kenpom teams))
                                       reach-probabilities (map :reach-probability outcome)
                                       outcome-probability (apply * (cons win-probability reach-probabilities))]
                                   {:team (first teams) :reach-probability outcome-probability}))
                               outcomes)

        ;; Group the results by team and sum up
        outcomes-grouped-by-team (vals (group-by (comp :name :team) outcome-probabilities))
        reduced-outcomes (map (fn [team-group]
                                (let [total-probability (apply + (map :reach-probability team-group))]
                                  (assoc (first team-group) :reach-probability total-probability)))
                              outcomes-grouped-by-team)]

    ;; Limit to just the most likely outcome probs
    {:reach-distribution (take limit (sort-by :reach-probability > reduced-outcomes))}))

(defn- make-single-team-reach-distribution [team]
  {:reach-distribution (list {:team team :reach-probability 1.0})})

;; TODO pass limit in to this
(defn- find-bracket-reach-distributions [limit sub-bracket]
  (if (seq? sub-bracket)
    (let [sub-bracket-results (map (partial find-bracket-reach-distributions limit) sub-bracket)]
      (play-reach-distributions limit sub-bracket-results))
    (let [team sub-bracket]
      (make-single-team-reach-distribution team))))


(def -main
  (partial find-bracket-reach-distributions 4 full-bracket))
