(ns ncaa-bracket-2015.core
  (:gen-class))

(defn- pvictory [pythag-winner pythag-loser]
  (let [a pythag-winner
        b pythag-loser]
    (/ (- a (* a b))
       (+ a b (* -2 a b)))))



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
