(ns rhombrick.tileset-library
  (:use [clojure.java.io]))


(def default-library-file "data/tilesets.clj")


(defn save-tileset-to-library 
  ([tileset]
    (save-tileset-to-library tileset default-library-file))
  ([tileset filename]
    (with-open [wrtr (writer filename :append true)]
      (.write wrtr (str (.toString tileset) "\n")))))


(defn parse-lines [lines acc]
  (if-let [line (first lines)]
    (recur (rest lines) (conj acc (read-string line)))
    acc))


(defn load-tileset-library
  ([] (load-tileset-library default-library-file))
  ([filename]
    (with-open [rdr (reader filename)]
      (parse-lines (line-seq rdr) []))))



