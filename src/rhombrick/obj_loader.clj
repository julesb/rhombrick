(ns rhombrick.obj-loader
  (:use [clojure.java.io]
        [clojure.string :only (split)]))


(defn get-line-type [line]
  (cond
    (re-find #"^v " line)   :vertex
    (re-find #"^vn " line)  :vertex-normal
    (re-find #"^vt " line)  :texture-coordinate
    (re-find #"^f " line)   :face
    (re-find #"^#" line)    :comment
    (re-find #"^o " line)   :object
    :else :unsupported))


(defn parse-vertex-line [line]
  (let [data (str "[" ((re-find #"^v (.+)$" line) 1) "]")]
    (read-string data)))


(defn parse-vertex-normal-line [line]
  (let [data (str "[" ((re-find #"^vn (.+)$" line) 1) "]")]
    (read-string data)))


(defn parse-texture-coordinate-line [line]
  (let [data (str "[" ((re-find #"^vt (.+)$" line) 1) "]")]
    (read-string data)))


; face data can be in various formats:
;
; f v1 v2 v3 v4
; f v1/vt1 v2/vt2 v3/vt3 v4/vt4
; f v1//vn1 v2//vn2 v3//vn3 v4//vn4
;
; v = vertex index
; vt = tex coord index
; vn = vertex normal index
;
; Currently ignoring tex coords and normals as they are not needed just now.
; Just returning an array of vertex idxs: [1 2 3 4]

(defn parse-face-token [tok]
  (->> (split tok #"/")
       (map #(if (= "" %) "-1" %))
       (map read-string)
       (vec)))


(defn parse-face-line [line]
  (->> (rest (clojure.string/split line #" "))
       (map parse-face-token)
       (map first) ; only using face index, ignore others
       (vec)))


(defn parse-comment-line [line]
  (let [data ((re-find #"^#(.*$)" line) 1)]
    data))


(defn parse-object-line [line]
  (let [data (str ((re-find #"^o (.+)$" line) 1))]
    (read-string data)))


(defn parse-unsupported-line [line])


(def type-func-map {
  :vertex             parse-vertex-line
  :vertex-normal      parse-vertex-normal-line
  :texture-coordinate parse-texture-coordinate-line
  :face               parse-face-line
  :comment            parse-comment-line
  :object             parse-object-line
  :unsupported        parse-unsupported-line
})


(defn parse-line [line]
  ((type-func-map (get-line-type line)) line))


(defn parse-file [lines acc]
  (if-let [line (first lines)]
    (let [line-type (get-line-type line)
          new-data (into [] (conj (acc line-type) (parse-line line)))
          new-acc (assoc acc line-type new-data)]
      (recur (rest lines) new-acc))
    acc))


(defn load-obj
  [filename]
  "Load data from a Wavefront/.obj file into a map with entries"
  ":vertex, :face etc. Keys are defined in get-line-type"
  (with-open [rdr (reader filename)]
    (parse-file (line-seq rdr) {})))


(defn load-obj-test [filename]
  (with-open [rdr (reader filename)]
    (doseq [line (line-seq rdr)]
      (when-not (= (get-line-type line) :unsupported)
        ;(println "LINE:" line)
        (println (get-line-type line) (str (parse-line line)))))))

