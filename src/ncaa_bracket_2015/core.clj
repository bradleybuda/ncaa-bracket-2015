(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(ns ncaa-bracket-2015.core
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

(defn- fill-using-layout [base-fn layout]
  (map
   (if (seq? (first layout))
     (partial fill-using-layout base-fn) ;; recurse
     base-fn ;; base case
     ) layout))

(defn- region-bracket [region layout]
  (fill-using-layout
   (fn [seed]
     (first (filter (fn [team]
                      (and (= seed (:seed team))
                           (= region (:region team))))
                    (teams-and-seeds))))
   layout))

(defn- bracket [layout]
  (fill-using-layout
   (fn [region] (region-bracket region region-layout))
   layout))

(def full-bracket (partial bracket final-four))

(defn- teams-and-seeds []
  (with-open [in-file (io/reader "data/bracket-00.tsv")]
    (let [records (seq (doall (csv/read-csv in-file :separator \tab)))]
      (map
       (fn [record]
         (let [[_ name seed region kenpom] record]
           ; TODO parse seed into numeric and play-in status
           {:name name :seed (read-string (re-find #"\d+" seed)) :region region :kenpom (read-string kenpom)}
           ))
       (rest records)))))

(defn- pvictory [pythag-winner pythag-loser]
  (let [a pythag-winner
        b pythag-loser]
    (/ (- a (* a b))
       (+ a b (* -2 a b)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
