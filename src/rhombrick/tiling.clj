(ns rhombrick.tiling
  (:use [rhombrick.vector]
        [rhombrick.staticgeometry]
        [rhombrick.facecode]
        [ordered.map]))

;(use 'ordered.map)

(def max-tiles 100)
;(def tiles (atom {}))
(def tiles (atom (ordered-map)))


(def working-tileset (atom #{
                      ;"100000000000"
                      ;"100000000100" 
                      "110000000000"
                      ;"000111000000"
                      ;"000101010000"
                      ;"100010001000"
                      ;"111111111111"
                      }))

(def facecode-compatible #{[\0 \0] [\1 \1] [\0 \-] [\1 \-] [\- \-]})

(def auto-delete-max-lonlieness (atom 1))

(def face-list (atom #{}))



 ;(filter #(and (has-neighbours? %) (tileable? %)) (keys @tiles)))



(def untilable-codes (atom #{}))

(defn init-untilable-codes []
  (reset! untilable-codes #{}))

(defn add-to-untilable-codes [code]
  (swap! untilable-codes conj code))

; _______________________________________________________________________

(def todo (atom (clojure.lang.PersistentQueue/EMPTY)))

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

; vvv slow vvv
(defn delete-todo-item [pos]
  (reset! todo 
          (into (clojure.lang.PersistentQueue/EMPTY)
                (filter #(not= % pos) @todo))))



; _______________________________________________________________________

(defn get-n-rand-tilecode [n]
  (vec 
    (map (fn [a] (rand-nth (take 352 (seq @normalised-facecodes-sorted))))
         (range n))))

(defn get-n-rand-tilecode-from-group [n g]
  (map (fn [a] (rand-nth (@normalised-facecodes-grouped g)))
         (range n)))

(defn random-tileset-from-groups []
  (reset! working-tileset #{})
  (doseq [code (get-n-rand-tilecode-from-group 0 1) ]
      (swap! working-tileset conj code))
  (doseq [code (get-n-rand-tilecode-from-group 0 2) ]
      (swap! working-tileset conj code))
  (doseq [code (get-n-rand-tilecode-from-group 1 3) ]
      (swap! working-tileset conj code))
  (doseq [code (get-n-rand-tilecode-from-group 0 4) ]
      (swap! working-tileset conj code))
  (doseq [code (get-n-rand-tilecode-from-group 0 5) ]
      (swap! working-tileset conj code))
  (doseq [code (get-n-rand-tilecode-from-group 0 8) ]
      (swap! working-tileset conj code))
  (doseq [code (get-n-rand-tilecode-from-group 0 12) ]
      (swap! working-tileset conj code))
  (println "working tileset: " @working-tileset)
  )


(defn random-tileset []
  (let [num-tiles (+ 1 (rand-int 5))]
    (reset! working-tileset #{})
    ;(swap! working-tileset conj "100000000000")
    (doseq [code (get-n-rand-tilecode num-tiles) ]
      (swap! working-tileset conj code))
    (println "working tileset: " @working-tileset)))
 


(defn random-tileset-old []
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

;(defn auto-seed-todo []
;  (if (= (count @todo) 0)
;    (do
;      (init-tiler)
;      ;(random-tileset)
;      )))
; _______________________________________________________________________

(defn get-neighbour [pos face]
  (vec3-add pos (rd-neighbour-offsets face)))

(defn get-neighbours [pos]
  (vec (map #(get-neighbour pos %) (range 12))))


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


;(defn get-connected-idxs [facecode]
;  (filter #(not= nil %)
;          (map #(if (= %2 \1) %1 nil)
;               (range 12) facecode)))


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


; determine if faces are compatible without rotation
(defn facecodes-directly-compatible? [outercode innercode]
  (= 12 
     (count (filter #(true? %)
                    (map #(face-digit-compatible? %1 %2) 
                         innercode outercode)))))


(defn find-candidates [pos tileset]
  (let [outercode (get-outer-facecode pos)]
    (filter #(facecodes-directly-compatible? outercode %)
            (expand-tiles tileset))))
  

(defn choose-tilecode-old [pos tileset]
  (let [candidates (find-candidates pos tileset)]  
    (if (seq candidates)
      (nth candidates (rand-int (count candidates)))
      "xxxxxxxxxxxx")))
  

(defn choose-tilecode [pos tileset]
  (let [candidates (find-candidates pos tileset)
        candidates-filtered (filter #(> (count (get-connected-idxs %)) 1)
                                    candidates)]  
    (if (seq candidates-filtered)
      (nth candidates-filtered (rand-int (count candidates-filtered)))
      (if (seq candidates)
        (nth candidates (rand-int (count candidates)))
        "xxxxxxxxxxxx"))))
; _______________________________________________________________________




(def empty-positions (atom #{}))

(defn init-empty-positions []
  (reset! empty-positions #{}))

(defn add-to-empty-positions [pos]
  (if (< (+ (count @empty-positions)
            (count @tiles))
         max-tiles)
    (swap! empty-positions conj pos)))


(defn remove-from-empty-positions [pos]
  (swap! empty-positions disj pos))

(defn push-neighbours-to-empty-positions [pos]
  (dotimes [face 12]
    (let [neighbour (get-neighbour pos face)]
      (if (tileable? neighbour)
          (add-to-empty-positions neighbour)))))

(defn update-empty-positions []
  (do
    (init-empty-positions)
    (doseq [tile (keys @tiles)]
      (doseq [n (get-neighbours tile)]
        (if (tileable? n)
          (add-to-empty-positions n))))))


; _______________________________________________________________________

;(def face-list (atom #{}))


(defn face-idxs-to-verts [face-idxs]
  (vec (map #(rd-verts %) face-idxs)))

(defn facelist-contains-rotations? [face-verts]
  (or
    (> (count (filter #(contains? @face-list %)
                      (rotations-vec face-verts)))
       0)
    (> (count (filter #(contains? @face-list %)
                      (rotations-vec (vec (reverse face-verts)))))
       0)   
       ))


(defn remove-from-facelist [face-verts]
    (let [face-rots (concat (rotations-vec face-verts) 
                            (rotations-vec (reverse face-verts)))]
        (doseq [f face-rots]
          (if (contains? @face-list f)
            (swap! face-list disj f)))))
      
  

; we dont need to check every face in the face list here
; only need to check the neighbours.
(defn add-tile-to-facelist [pos]
  (doseq [f rd-faces]
    (let [fv (face-idxs-to-verts f)
          fvw (vec (map #(vec3-add pos (vec3-scale % 0.5)) fv))]
    (if (not (facelist-contains-rotations? fvw))
      (swap! face-list conj fvw)
      (do
        (remove-from-facelist fvw))))))



; this builds the entire face list based on the contents of @tiles
; very slow
;(defn build-face-list []
;  (reset! face-list #{})
;  (doseq [tile-pos (keys @tiles)]
;    (doseq [i (range 12)]
;      (let [face-verts (face-idxs-to-verts (rd-faces i))
;            actual-verts (vec (map #(vec3-add tile-pos 
;                                              (vec3-scale % 0.5))
;                                   face-verts))]
;        (if (not (facelist-contains-rotations? actual-verts))
;          (swap! face-list conj actual-verts))))))
; 



; _______________________________________________________________________


(defn seed-tiler []
  (let [pos [0 0 0]
        code (choose-tilecode pos @working-tileset)]
    (make-tile pos code) 
    (push-neighbours-to-empty-positions pos)))

; _______________________________________________________________________


(defn init-tiler []
  (reset! tiles {})
  ;(init-todo)
  (init-empty-positions)
  (init-untilable-codes)
  (seed-tiler)
  (reset! face-list #{}))


; _______________________________________________________________________



; this does facecode constrained tiling
(defn make-tiling-iteration []
  (if (and
        (seq @todo)
        (< (+ (count @todo) (count @tiles)) max-tiles ))
    (let [new-pos (dequeue-todo)
          new-code (choose-tilecode new-pos @working-tileset)]
      (if (and (not= new-code "xxxxxxxxxxxx")
               (= (count new-code) 12))
        (do
          (make-tile new-pos new-code)
          (add-tile-to-facelist new-pos)
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
;
; Backtracking tiler algorithm
; ============================
;
; 1. Place a random tile in the center of the grid
;
; 2. Make a list of empty locations in the grid with abutting non-empty 
; edges. If there are no such locations, halt. 
;
; Otherwise, if there are any sites where only either one or zero 
; types of tile could be added, restrict the list to just these sites.
; From the list, choose the location closest to the center of the
; assemblage. 

; 3. If there is no tile that fits at that location, or if it can be
; determined that for any tile that might be added the assemblage will
; become non-completable (see next section), perform backtracking. That
; is, remove some number of tiles from the assemblage in the reverse
; order to which they were added (see the section after next).
;
; 4. Otherwise choose a tile at random from the remaining possibilities,
; and put it at the location.
;
; 5. Go to step 2.


; Todo:
; - Replace the todo persistent queue with a set called say abutting-spaces,
; Ordering doesnt matter, the items are unique, and we need to be able to
; remove arbitary items.
;
; - Replace @tiles which is currently a map, with something. We need to
; preserve ordering so we can backtrack. clojure.lang.PersistentQueue
; should do the job as we can just pop/dequeue when backtracking.
;
; Try these - Ordered sets and maps: https://clojars.org/search?q=ordered
;
;
; - Implement a untilable-loci set, and when choosing a tile for a location,
; if there is no matching tile, then add that locus (referred to as 
; outer-facecode in previous tiler algorithm) to the untilable set. Also
; when adding a tile, check if any locations which match entries in the
; untilable set are created by adding the tile, and if so, choose a
; different tile if possible.
;
; - Implement a random generator with power law distribution for choosing
; the amount to backtrack:
; 
;                         P(n) ∝ (n + a)^−b
;
; where n is the number of tiles to remove (n > 0, integer), and the
; parameters a and b define the personality of the assembler. The parameters
; affect how long the assembler will take to finish, and may also bias the
; result towards the emergence of one or another kind of feature. The
; parameter values used in this chapter were a = 0.35, b = 3.
;




; _______________________________________________________________________


; returns a list of todo locations with 0 or 1 matching tiles
; (this is according to the paper, but I dont understand the point
; of returning locations with 0 matching tiles)

(defn find-best-positions []
  (filter #(< (count (find-candidates % @working-tileset)) 2)
          @empty-positions))

; returns a list of todo locations with any matching tiles
(defn find-any-positions []
  (filter #(> (count (find-candidates % @working-tileset)) 0)
          @empty-positions))

; _______________________________________________________________________

; receive a vector of positions and return the closest to the center
; ie the vector with the shortest length. If there are more than one
; equal to the shortest then return a random one. 
(defn find-closest-to-center [positions]
  (let [lengths (into {} (map #(vec [%1 (vec3-length %1)]) positions))
        sorted-lengths (sort-by #(vec3-length (key %)) lengths)
        min-length ((first sorted-lengths) 1)
        tie-winners (filter #(= min-length (val %)) sorted-lengths) ]
    (if (= 1 (count tie-winners))
      ((first tie-winners) 0)
      ((rand-nth tie-winners) 0))))

; _______________________________________________________________________

(def autism 0.35)
(def adhd 2.5)

(defn get-backtrack-amount []
  (let [t (count @tiles)]
    (loop [n 1]
      (if (or (> n (- t 1))
              (> (rand)
                 (Math/pow (/ n (+ n autism)) adhd)))
        n
        (recur (inc n))))))




(defn backtrack []
  (let [num-tiles (count @tiles)
        n (get-backtrack-amount)
        ;n (+ 1 (rand-int (/ num-tiles 4)))
        ni (- num-tiles n)]
    (println "tilecount:" num-tiles ", backtracking" n "tiles") 
    ; remove n most recent @tiles
    (reset! tiles (ordered-map (take ni @tiles)))
    ; rebuild empty postion list
    (update-empty-positions)))
    



(defn make-backtracking-tiling-iteration []
  (when (and (< (count @tiles) max-tiles)
             (> (count @empty-positions) 0))
    (let [best-positions (find-best-positions)]
      (if (> (count best-positions) 0)
        ; we have one or more locations with 0 or 1 matches
        (let [new-pos (find-closest-to-center best-positions)
              new-code (choose-tilecode new-pos @working-tileset)]
          ; choose tile and add to chosen position
          (if (and (not= new-code "xxxxxxxxxxxx")
                   (= (count new-code) 12))
            (do
              (make-tile new-pos new-code)
              (add-tile-to-facelist new-pos)
              (remove-from-empty-positions new-pos)
              (push-neighbours-to-empty-positions new-pos)
              )

            (do
              ;(make-tile new-pos "xxxxxxxxxxxx")
              (add-to-untilable-codes (get-outer-facecode new-pos))
              ; do backtracking
              (backtrack)
              ))
        )
        ; else there are no positions with 0 or 1 (best) candidates, 
        ; so see if the positions have other number of candidates
        (let [positions (find-any-positions)]
            (if (> (count positions) 0)
              (let [new-pos (find-closest-to-center positions)
                    new-code (choose-tilecode new-pos @working-tileset)]
                ; choose tile and add to chosen position)
                (if (and (not= new-code "xxxxxxxxxxxx")
                         (= (count new-code) 12))
                  (do
                    (make-tile new-pos new-code)
                    (add-tile-to-facelist new-pos)
                    (remove-from-empty-positions new-pos)
                    (push-neighbours-to-empty-positions new-pos)
                    )
                  (do
                    ;(make-tile new-pos "xxxxxxxxxxxx")
                    (add-to-untilable-codes (get-outer-facecode new-pos))
                    ; do backtracking
                    (backtrack)
                    )) 
              )))))))


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


