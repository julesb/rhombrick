(defproject rhombrick "1.0.0-SNAPSHOT"
  :description "Rhombic dodecahedral tiling engine"
  :dependencies [
                 [org.clojure/clojure "1.6.0"]
                 ;[org.clojure/clojure "1.5.1"]
                 ;[quil "1.7.0"]
                 ;[quil "2.0.0-SNAPSHOT"]
                 ;[quil "2.0.0"]
                 [quil "2.2.4"]
                 ;[quil "2.3.0"]
                 ;[quil "1.6.0"]
                 ;[quil "1.6.0-SNAPSHOT"]
                 ;[overtone/osc-clj "0.7.1"]
                 [ordered "1.3.2"]
                 [org.clojure/math.combinatorics "0.1.1"]
                 ;[org.clojure/core.logic "0.6.5"]
                 ;[org.clojure/java.jdbc "0.3.0-alpha4"]
                 ;[mysql/mysql-connector-java "5.1.6"]
                 [org.clojure/core.memoize "0.5.6"]
                 [scad-clj "0.3.0"]
                 ;[org.clojure/math.numeric-tower "0.0.4"]
                 ]
  :main rhombrick.core
  :profiles {:uberjar {:aot :all}}
  ;:warn-on-reflection true
  :jvm-opts ["-Xmx2g"]
  )

