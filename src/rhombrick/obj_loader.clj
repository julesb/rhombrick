(ns rhombrick.obj-loader
  (:use [clojure.java.io]
        [clojure.string :only (split)]))


(defn parse-vertex-line [line]
  (->> (str "[" ((re-find #"^v (.+)$" line) 1) "]")
       (read-string)))


(defn parse-vertex-normal-line [line]
  (->> (str "[" ((re-find #"^vn (.+)$" line) 1) "]")
       (read-string)))


(defn parse-texture-coordinate-line [line]
  (->> (str "[" ((re-find #"^vt (.+)$" line) 1) "]")
       (read-string)))


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
       (map dec) ; obj format seems to use 1-based index for face verts?
       (vec)))


(defn parse-face-line [line]
  (->> (rest (clojure.string/split line #" "))
       (map parse-face-token)
       (map first) ; only using face index, ignore others
       (vec)))


(defn parse-comment-line [line]
  ((re-find #"^#(.*$)" line) 1))


(defn parse-object-line [line]
  (->> (str ((re-find #"^o (.+)$" line) 1))
       (read-string)))


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


(defn get-line-type [line]
  (cond
    (re-find #"^v " line)   :vertex
    (re-find #"^vn " line)  :vertex-normal
    (re-find #"^vt " line)  :texture-coordinate
    (re-find #"^f " line)   :face
    (re-find #"^#" line)    :comment
    (re-find #"^o " line)   :object
    :else :unsupported))


(defn parse-line [line]
  ((type-func-map (get-line-type line)) line))


(defn parse-file [lines acc]
  (if-let [line (first lines)]
    (let [line-type (get-line-type line)]
      (->> (into [] (conj (acc line-type) (parse-line line)))
           (assoc acc line-type)
           (recur (rest lines))))
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



(defn get-verts [verts face]
  (vec (map #(verts %) face)))


(defn get-obj-face-verts [obj]
  (->> (obj :face)
       (map #(get-verts (obj :vertex) %))
       (vec)))

