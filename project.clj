(defproject rhombrick "1.0.0-SNAPSHOT"
  :description "Rhombic dodecahedral tiling engine"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 ;[quil "1.4.1"]
                 [quil "1.6.0"]
                 ;[quil "1.6.0-SNAPSHOT"]
                 [overtone/osc-clj "0.7.1"]
                 [ordered "1.3.0"]
                 [org.clojure/math.combinatorics "0.0.3"]
                 [org.clojure/core.logic "0.6.5"]
                 [org.clojure/java.jdbc "0.3.0-alpha4"]
                 [mysql/mysql-connector-java "5.1.6"]
                 ]
  :main rhombrick.core
  :warn-on-reflection true)

