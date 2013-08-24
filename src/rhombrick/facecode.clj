(ns rhombrick.facecode
  (:use [rhombrick.vector]
        [rhombrick.staticgeometry]
        [clojure.math.combinatorics]))

; _______________________________________________________________________
;
; Face code logick
;


; This defines the index of the abutting face for a given neighbouring block.
; eg. If a neighbour block is abutting face 0, then the abutting face
; on the neighbouring block is 9. This essentially returns the index
; of the opposite face.

(def connecting-faces-orig [9 10 11 6 7 8 3 4 5 0 1 2])

 (def connecting-faces [6 7 8 9 10 11 0 1 2 3 4 5])


; for testing
(def test-facecode "101111000010")



(defn make-n-bits-long [bits n]
  (concat (repeat (- n (count bits)) 0) bits))


; Generate a list of left zero padded binary representations of all
; integers from 0 to 2^12 - 1
(defn generate-face-codes []
  (map #(apply str (make-n-bits-long (Integer/toString %1 2) 12))
       (range 4096)))



(defn rotate-vec [v]
  (vec (concat (rest v) [(first v)])))

(defn rotations-vec [v]
    (loop [n (count v)
           accum [v]]
      (if (> n 1)
        (recur (dec n) (conj accum (rotate-vec (last accum))))
        (vec accum))))

; rotate a string one char to the left
(defn rotate-str [s]
  (apply str (concat (rest s) [(first s)])))


; generate a list of rotations of a string
(defn rotations [s]
    (loop [n (count s)
           accum [s]]
      (if (> n 1)
        (recur (dec n) (conj accum (rotate-str (last accum))))
        accum)))

(defn rotate-str-n [s n]
  (nth (rotations s) n))


;(defn rotations [s]
;  (map #(rotate-str-n s %1) (range (count s))))


(defn reverse-tiles [tiles]
  (map #(apply str (reverse %)) tiles))

(defn is-facecode-rotation-of? [a b]
  (boolean (some #{a} (rotations b))))


;(defn get-connected-idxs [facecode]
;  (filter #(not= nil %)
;          (map #(if (= %2 \1) %1 nil)
;               (range 12) facecode)))

(defn get-connected-idxs [facecode]
  (filter #(not= nil %)
          (map #(if (and (not= %2 \-) (not= %2 \0) ) %1 nil)
               (range 12) facecode)))


;(defn get-nonconnected-idxs [facecode]
;  (filter #(not= nil %)
;          (map #(if (not (= %2 \1)) %1 nil)
;               (range 12) facecode)))


(defn get-nonconnected-idxs [facecode]
  (filter #(not= nil %)
          (map #(if (= %2 \-) %1 nil)
               (range 12) facecode)))



(def all-facecodes (generate-face-codes))
(def normalised-facecodes (atom {}))
(def normalised-facecodes-set (atom #{}))
(def normalised-facecodes-sorted (atom []))
(def normalised-facecodes-grouped (atom {}))

; returns true if the given facecode or any of its rotations
; exists in the normalised set, otherwise false.
(defn is-in-normalised-set? [facecode]
  (> (count (filter true? 
                    (map #(contains? @normalised-facecodes-set %1)
                         (rotations facecode))))
     0))


(defn set-contains-rotations? [s code]
  (> (count (filter true? 
                    (map #(contains? s %1)
                         (rotations code))))
     0))


; Group by number of connected edges. The distribution of rotation invariant
; tiles for number of connections happens to have an interesting symmetry:
; (doseq [g (keys tilegroups)] 
;   (println g (count (tilegroups g)))) 
; 0 1
; 1 1
; 2 6
; 3 19
; 4 43
; 5 66
; 6 80
; 7 66
; 8 43
; 9 19
; 10 6
; 11 1
; 12 1

(defn group-by-num-connected [facecodes]
  (group-by #(count (get-connected-idxs %)) facecodes))

(defn make-tilegroups []
  (group-by-num-connected @normalised-facecodes-sorted))

;
; Construct a set consisting of a subset of all possible facecodes.
; The new set includes only facecodes which are are unique, even
; under rotation. For example the facecodes "000000000001",
; "000000000010" and "000001000000" are all considered to be
; equal, since they are all rotations of "000000000001", thus
; only the first on is added to the normalised set.
;
; After filtering in this manner, 352 facecodes remain out of the
; 4096 possible facecodes
;

(defn build-normalised-facecode-set []
  (do
    (doseq [code all-facecodes]
      (if (not (is-in-normalised-set? code))
        (swap! normalised-facecodes-set conj code)))
    (reset! normalised-facecodes-sorted 
            (vec (sort @normalised-facecodes-set)))
    (reset! normalised-facecodes-grouped
            (make-tilegroups))
    nil))
 


(defn get-angle-for-face-idxs [[idx1 idx2]]
  (vec3-angle-between (vec3-normalize (co-verts idx1))
                      (vec3-normalize (co-verts idx2))))


(defn get-tilecode-angles [code]
  (->> (map-indexed #(vec [%1 %2]) code)
       (filter #(not= (%1 1) \-))
       (map first)
       ((fn [v] (vec (combinations v 2))))
       (map get-angle-for-face-idxs)
       (map int)))


(defn get-tilecode-angle-ids [code]
  (vec (sort (get-tilecode-angles code))))


;(defn rotations-preserving-symmetry [code]
;  (let [rots (map-indexed #(vec [%1 %2])
;                          (distinct (rotations code)))
;        symmetries (vec (map-indexed #(vec [%1 (get-tilecode-angle-ids (%2 1))])
;                                     rots))
;        canonical-form ((first symmetries) 1)
;        indexed (filter #(= ((symmetries (% 0)) 1) canonical-form) rots)
;        flat (vec (map last indexed))]
;    flat
;    ))

  
