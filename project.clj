(defproject ncaa-bracket-2015 "0.1.0-SNAPSHOT"
  :url "https://github.com/bradleybuda/ncaa-bracket-2015"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.csv "0.1.2"]
                 [org.clojure/math.combinatorics "0.1.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]]
  :main ^:skip-aot ncaa-bracket-2015.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
