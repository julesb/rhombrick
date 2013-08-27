(ns rhombrick.tilebase
  (:use [rhombrick.tiling]
        [rhombrick.vector]
        [rhombrick.staticgeometry :as geom]
        [rhombrick.facecode]
        [ordered.map]
        [clojure.java.jdbc :only [with-connection with-query-results insert-values]]
        )
  )
  ;(require '[clojure.java.jdbc :as db])

(def db-host "127.0.0.1")
(def db-port 3199)
(def db-name "rhombrick")
(def db-user "root")
(def db-pass "mysql-root")


(def db
  {:classname "com.mysql.jdbc.Driver"
   :subprotocol "mysql"
   :subname (str "//" db-host ":" db-port "/" db-name)
   :user db-user
   :password db-pass} )


(defn test-query [db]
  (with-connection db 
    (with-query-results rs ["select * from wp_users"] 
     ; rs will be a sequence of maps, 
     ; one for each record in the result set. 
     (dorun (map #(println (:user_login %)) rs)))))


(defn insert-tiling-result [res]
  (clojure.java.jdbc/insert-values
    :tiling
      [:tileset_number 
       :tileset
       :seed
       :max_tiles
       :max_iters
       :max_radius
       :best_of
       :autism
       :adhd
       :tiles
       :tilecount
       :iters_done
       ]
      [(str ((res :params) :tileset-number))
       (str ((res :params) :tileset))
       (str ((res :params) :seed))
       ((res :params) :max-tiles)
       ((res :params) :max-iters)
       ((res :params) :max-radius)
       ((res :params) :best-of)
       ((res :params) :autism)
       ((res :params) :adhd)
       (pr-str ((res :result) :tiling))
       ((res :result) :tilecount)
       ((res :result) :iters-done)
       ]))

(defn save-tiling-result [result]
  (clojure.java.jdbc/with-connection
    db
    (clojure.java.jdbc/transaction
      (insert-tiling-result result ))
  ))



