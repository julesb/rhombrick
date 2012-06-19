(ns rhombrick.tiling
  (:use [rhombrick.vector]
        [rhombrick.staticgeometry]
        [rhombrick.facecode]))

(def max-tiles 1000)
(def tiles (atom {}))
(def todo (atom (clojure.lang.PersistentQueue/EMPTY)))

(def working-tileset (atom #{
                      "100000001000" 
                      ;"000111000100"
                      ;"100100100100"
                      }))

(def facecode-compatible #{[\0 \0] [\1 \1] [\0 \-] [\1 \-] [\- \-]})

(def auto-delete-max-lonlieness (atom 1))


    
; _______________________________________________________________________


(defn init-todo []
  (do
    (reset! todo (clojure.lang.PersistentQueue/EMPTY))
    (swap! todo conj [0 0 0])))

(defn pop-todo []
  (if (seq @todo)
    (let [top (last @todo)]
      (swap! todo pop )
      top)))

(defn dequeue-todo []
  (if (seq @todo)
    (let [top (peek @todo)]
      (swap! todo pop)
      top)))

(defn in-todo? [pos]
  (boolean (some #{pos} @todo)))

(defn push-todo [pos]
  (if (not (in-todo? pos))
    (swap! todo conj pos)))


; _______________________________________________________________________


(defn init-tiler []
  (reset! tiles {})
  (init-todo))

; _______________________________________________________________________


(defn random-tileset []
  (let [num-tiles 1
        tiles (repeat num-tiles 
                      (rand-nth 
                        (take 50 (seq @normalised-facecodes-sorted))))]
        ;(init-tiler)
        (reset! working-tileset #{})
        (doseq [tile tiles]
          (swap! working-tileset conj tile))
        (if (< (rand-int 100) 90)
          (swap! working-tileset conj "100000000000"))
        (if (< (rand-int 100) 50)
          (swap! working-tileset conj "111111111111"))
        (if (< (rand-int 100) 50)
          (swap! working-tileset conj "110000000000"))
        (if (< (rand-int 100) 30)
          (swap! working-tileset conj "101000000000"))
        (if (< (rand-int 100) 30)
          (swap! working-tileset conj "100100000000"))
        (if (< (rand-int 100) 30)
          (swap! working-tileset conj "100010000000"))
        (if (< (rand-int 100) 30)
          (swap! working-tileset conj "100000000000"))

        ))

(defn auto-seed-todo []
  (if (= (count @todo) 0)
    (do
      (init-tiler)
      ;(random-tileset)
      )))
; _______________________________________________________________________

(defn get-neighbour [pos face]
  (vec3-add pos (rd-neighbour-offsets face)))


(defn tileable? [pos]
  (not (contains? @tiles pos)))


(defn make-tile [pos facecode]
  (if (tileable? pos)
    (swap! tiles assoc pos facecode)))


(defn delete-tile [pos]
  (swap! tiles dissoc pos))


(defn delete-random-tile []
  (let [to-delete (rand-nth (keys @tiles))]
    (swap! tiles dissoc to-delete)))


(defn neighbour-states [pos]
  (map #(not (tileable? (vec3-add pos %1)))
       rd-neighbour-offsets))


(defn neighbour-count [pos]
  (count (filter #(true? %) (neighbour-states pos))))


(defn has-neighbours? [pos]
  (not (zero? (neighbour-count pos))))


(defn delete-nonconnected-tiles []
  (doseq [tile (keys @tiles)]
    ;(if (not (has-neighbours? tile))
    (if (< (neighbour-count tile) @auto-delete-max-lonlieness)
      (delete-tile tile))))


(defn delete-fully-connected-tiles []
  (let [fc-tiles (filter #(= (neighbour-count %1) 12) (keys @tiles))]
    (doseq [tile fc-tiles]
      (delete-tile tile))))


(defn push-neighbours-todo [pos]
  (dotimes [face 12]
    (let [neighbour (get-neighbour pos face)]
      (if (tileable? neighbour)
          (push-todo neighbour)))))


(defn get-connected-idxs [facecode]
  (filter #(not= nil %)
          (map #(if (= %2 \1) %1 nil)
               (range 12) facecode)))


(defn push-connected-neighbours-todo [pos]
  (doseq [idx (get-connected-idxs (@tiles pos))]
    (let [neighbour (get-neighbour pos idx)]
      (if (tileable? neighbour)
          (push-todo neighbour)))))


(defn get-neighbour-abutting-face [pos face]
    (let [op-face (connecting-faces face)
          nb-pos (get-neighbour pos face)
          ;nb-code (@tiles nb-pos)
          nb-face-idx op-face ]
      (if (tileable? nb-pos)
        \-
        (nth (@tiles nb-pos) nb-face-idx))))


(defn get-outer-facecode [pos]
  (apply str (map #(get-neighbour-abutting-face pos %) (range 12))))


; generate all unique rotations of tiles 
; intended to be be used on the working tileset
; when choosing a tile
(defn expand-tiles [tiles]
  (set (flatten (map #(rotations %) tiles))))


; compares single digits of two facecodes, using the
; compatibility table 
(defn face-digit-compatible? [a b]
  (or (contains? facecode-compatible [a b])
      (contains? facecode-compatible [b a])))


; determine if faces are compatible with rotation
(defn facecodes-directly-compatible? [outercode innercode]
  (= 12 
     (count (filter #(true? %)
                    (map #(face-digit-compatible? %1 %2) 
                         innercode outercode)))))


(defn find-candidates [pos tileset]
  (let [outercode (get-outer-facecode pos)]
    (filter #(facecodes-directly-compatible? outercode %)
            (expand-tiles tileset))))
  

(defn choose-tilecode [pos tileset]
  (let [candidates (find-candidates pos tileset)]  
    (if (seq candidates)
      (nth candidates (rand-int (count candidates)))
      "xxxxxxxxxxxx")))
  

; _______________________________________________________________________


; this does facecode constrained tiling
(defn make-tiling-iteration []
  (if (seq @todo)
    (let [new-pos (dequeue-todo)
          new-code (choose-tilecode new-pos @working-tileset)]
      (if (and (not= new-code "xxxxxxxxxxxx")
               (= (count new-code) 12))
        (do
          (make-tile new-pos new-code)
          (push-connected-neighbours-todo new-pos))
        (make-tile new-pos "xxxxxxxxxxxx")))))


; _______________________________________________________________________



; this does plain space filling with no facecode constraints
(defn make-tiling-iteration-basic []
  (if (seq @todo)
    (let [new-pos (dequeue-todo)]
      (make-tile new-pos "111111111111")
      (push-neighbours-todo new-pos))))

(defn rotate-str-n [s n]
  (nth (rotations s) n))


; _______________________________________________________________________








; _______________________________________________________________________
; _______________________________________________________________________
; _______________________________________________________________________
; _______________________________________________________________________
; _______________________________________________________________________

(defn make-cubic-tiling [xr yr zr]
  ;(reset! tiles {})
  (doseq [i (range (- 0 xr) xr)
          j (range (- 0 yr) yr)
          k (range (- 0 zr) zr)]
    (let [x (* i 1)
          y (* j 1)
          z (* k 1)
          modsum (+ (mod i 2) (mod j 2) (mod k 2)) ]
      (if (or (= modsum 1) (= modsum 3))
        (if (< (Math/random) 0.1)
          (make-tile [i j k] "000000000000"))))))







; _______________________________________________________________________


