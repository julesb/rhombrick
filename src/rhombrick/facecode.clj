(ns rhombrick.facecode)

; _______________________________________________________________________
;
; Face code logick
;


; This defines the index of the abutting face for a given neighbouring block.
; eg. If a neighbour block is abutting face 0, then the abutting face
; on the neighbouring block is 9. This essentially returns the index
; of the opposite face.

(def connecting-faces [9 10 11 6 7 8 3 4 5 0 1 2])




; for testing
(def test-facecode "101111000010")



(defn make-n-bits-long [bits n]
  (concat (repeat (- n (count bits)) 0) bits))


; Generate a list of left zero padded binary representations of all
; integers from 0 to 2^12 - 1
(defn generate-face-codes []
  (map #(apply str (make-n-bits-long (Integer/toString %1 2) 12))
       (range 4096)))


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

(defn is-facecode-rotation-of? [a b]
  (boolean (some #{a} (rotations b))))

(def all-facecodes (generate-face-codes))
(def normalised-facecodes (atom {}))
(def normalised-facecodes-set (atom #{}))
(def normalised-facecodes-sorted (atom []))

; returns true if the given facecode or any of its rotations
; exists in the normalised set, otherwise false.
(defn is-in-normalised-set? [facecode]
  (> (count (filter true? 
                    (map #(contains? @normalised-facecodes-set %1)
                         (rotations facecode))))
     0))


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
  (doseq [code all-facecodes]
    (if (not (is-in-normalised-set? code))
      (swap! normalised-facecodes-set conj code)))
  (reset! normalised-facecodes-sorted (vec (sort @normalised-facecodes-set))))


