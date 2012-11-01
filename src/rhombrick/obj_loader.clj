(ns rhombrick.obj-loader
  (:use [clojure.java.io]))


; This is the sort of structure that load-obj should return:
(def obj-struct
  {
    :verts [[0 0 0]
            [0 0 1]]
    :vert-normals [[0.707107 -0.707107 0.000000]
                   [0.707107 0.000000 0.707107]]
    :faces [[1 11 2 12]
            [6 9 2 11]]
  }


(defn parse-vertex-line [line])

(defn parse-vertex-normal-line [line])

(defn parse-face-line [line])

(defn parse-line [line])



(defn load-obj [filename]
  (with-open [rdr (reader filename)]
    (doseq [line (line-seq rdr)]
      (println line)))
  )

