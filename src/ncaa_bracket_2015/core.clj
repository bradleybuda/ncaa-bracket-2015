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

(defn- bracket [region layout]
  (let [lhs (first layout)]
    (map
     (if (seq? lhs)
       (partial bracket region)
       (fn [seed] (str seed " in the " region))
       )
     layout)))

(defn- teams-and-seeds []
  (with-open [in-file (io/reader "data/bracket-00.tsv")]
    (let [records (seq (doall (csv/read-csv in-file :separator \tab)))]
      (map
       (fn [record]
         (let [[_ name seed region kenpom] record]
           ; TODO parse seed into numeric and play-in status
           {:name name :seed seed :region region :kenpom (read-string kenpom)}))
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
