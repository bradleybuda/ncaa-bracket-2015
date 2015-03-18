(ns ncaa-bracket-2015.core
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.math.numeric-tower :refer [expt]]
            [clojure.math.combinatorics :refer [cartesian-product permutations]]
            [clojure.pprint :refer [pprint]])

  (:gen-class))

(def final-four-layout
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
           {:name name
            :seed (read-string (re-find #"\d+" seed))
            :region region
            :kenpom (read-string kenpom)
            :play-in (re-matches #"\d+[ab]" seed)
            }))
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
  (bracket final-four-layout))

(defn- pvictory [pythag-winner pythag-loser]
  (let [a pythag-winner
        b pythag-loser]
    (/ (- a (* a b))
       (+ a b (* -2 a b)))))

(defn- score-victory [round winner-seed loser-seed]
  "How much a victory is worth given the round and relative seeds. Note
   that round 0 is the 'opening round' played on Thursday / Friday - the
   play-in games are round -1 and the national championship game is round 5."
  (if (> 0 round)
    0
    (+  (expt 2 round)
        (max 0 (- winner-seed loser-seed)))))

(defn- score-outcome
  "Determine the probability and value of a particular round outcome, i.e.
  Team A defeats Team B in Round N."
  [round-score-fn outcome]
  (let [teams (map :team outcome)
        winner (first teams)
        win-probability (apply pvictory (map :kenpom teams))
        score (apply round-score-fn (map :seed teams))
        reach-probabilities (map :reach-probability outcome)
        outcome-probability (apply * (cons win-probability reach-probabilities))
        previous-score (:cumulative-ev (first outcome))
        cumulative-ev (* outcome-probability (+ score previous-score))]
    {:team winner :cumulative-ev cumulative-ev :reach-probability outcome-probability}))

(defn- play-reach-distributions
  "Take a pair of reach-distributions and compute all outcomes of a game between them"
  [reach-distributions]
  (let [round (:round (first reach-distributions))
        round-score-fn (partial score-victory round)
        distributions (map :reach-distribution reach-distributions)
        ;; Pair off all the teams
        pairings (apply cartesian-product distributions)
        ;; Blow up into a list with both sets of outcomes for a pair (team A wins, team B wins)
        outcomes (mapcat permutations pairings)
        ;; Find the probability of each outcome based on the probability of the team reaching the round and their relative pyth
        outcome-probabilities (map (partial score-outcome round-score-fn) outcomes)
        ;; Group the results by team and sum up
        outcomes-grouped-by-team (vals (group-by (comp :name :team) outcome-probabilities))
        reduced-outcomes (map (fn [team-group]
                                (let [total-probability (apply + (map :reach-probability team-group))
                                      total-ev (apply + (map :cumulative-ev team-group))]
                                  (assoc (first team-group)
                                         :reach-probability total-probability
                                         :cumulative-ev total-ev)))
                              outcomes-grouped-by-team)]

    {:round (inc round)
     :reach-distribution reduced-outcomes}))

(defn- make-single-team-reach-distribution [team]
  {:round (if (:play-in team) -1 0)
   :reach-distribution (list {:team team :cumulative-ev 0.0 :reach-probability 1.0})})

(defn- find-bracket-reach-distributions [sub-bracket]
  (if (seq? sub-bracket)
    (let [sub-bracket-results (map find-bracket-reach-distributions sub-bracket)]
      ;; TODO print best result for each sub-bracket
      (play-reach-distributions sub-bracket-results))
    (let [team sub-bracket]
      (make-single-team-reach-distribution team))))

(defn -main []
  (pprint (find-bracket-reach-distributions full-bracket)))
