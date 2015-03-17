(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(ns ncaa-bracket-2015.core
  (:gen-class))

(defn- teams-and-seeds []
  (with-open [in-file (io/reader "data/bracket-00.tsv")]
    (let [records (seq (doall (csv/read-csv in-file :separator \tab)))]
      (map
       (fn [record]
         (let [[_ name seed region kenpom] record]
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
