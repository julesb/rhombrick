(ns rhombrick.tiling
  (:use [rhombrick.vector]
        [rhombrick.staticgeometry]
        [rhombrick.facecode]
        [ordered.map]))

;(use 'ordered.map)

(def max-tiles 200)
;(def tiles (atom {}))
(def tiles (atom (ordered-map)))


(def working-tileset (atom #{
                      ;"100000000000"
                      "100000100000" 
                      ;"111000000000"
                      ;"000111000000"
                      ;"000101010000"
                      ;"100010001000"
                      ;"111111111111"
                      }))

(def facecode-compatible #{
  [\0 \0]
  [\0 \-]
  [\1 \1]
  [\1 \-]
  [\- \-]})

(def face-list (atom #{}))

;(def assemblage-center (atom [0 0 0]))





; _______________________________________________________________________

;(def todo (atom (clojure.lang.PersistentQueue/EMPTY)))
;
;(defn init-todo []
;  (do
;    (reset! todo (clojure.lang.PersistentQueue/EMPTY))
;    (swap! todo conj [0 0 0])))
;
;(defn pop-todo []
;  (if (seq @todo)
;    (let [top (last @todo)]
;      (swap! todo pop )
;      top)))
;
;(defn dequeue-todo []
;  (if (seq @todo)
;    (let [top (peek @todo)]
;      (swap! todo pop)
;      top)))
;
;(defn in-todo? [pos]
;  (boolean (some #{pos} @todo)))
;
;(defn push-todo [pos]
;  (if (not (in-todo? pos))
;    (swap! todo conj pos)))
;
;; vvv slow vvv
;(defn delete-todo-item [pos]
;  (reset! todo 
;          (into (clojure.lang.PersistentQueue/EMPTY)
;                (filter #(not= % pos) @todo))))
;
;

; _______________________________________________________________________

(defn get-n-rand-tilecode [n]
  (vec 
    (map (fn [a] (rand-nth (take 352 (seq @normalised-facecodes-sorted))))
         (range n))))

(defn get-n-rand-tilecode-from-group [n g]
  (map (fn [a] (rand-nth (@normalised-facecodes-grouped g)))
         (range n)))


(defn random-tileset-blah []
  (reset! working-tileset #{})
  (if (< (rand-int 100) 30)
    (swap! working-tileset conj (get-n-rand-tilecode-from-group 1 1)))
  (if (< (rand-int 100) 50)
    (swap! working-tileset conj (get-n-rand-tilecode-from-group 2 2)))
  (if (< (rand-int 100) 40)
    (swap! working-tileset conj (get-n-rand-tilecode-from-group 1 3)))
  (if (< (rand-int 100) 20)
    (swap! working-tileset conj (get-n-rand-tilecode-from-group 1 4)))
  (if (< (rand-int 100) 10)
    (swap! working-tileset conj (get-n-rand-tilecode-from-group 1 5)))
  (if (< (rand-int 100) 10)
    (swap! working-tileset conj (get-n-rand-tilecode-from-group 1 6)))
  (if (< (rand-int 100) 10)
    (swap! working-tileset conj (get-n-rand-tilecode-from-group 1 7)))
  (if (< (rand-int 100) 10)
    (swap! working-tileset conj (get-n-rand-tilecode-from-group 1 8)))
  (if (< (rand-int 100) 10)
    (swap! working-tileset conj (get-n-rand-tilecode-from-group 1 9)))
  (if (< (rand-int 100) 10)
    (swap! working-tileset conj (get-n-rand-tilecode-from-group 1 10)))
  (if (< (rand-int 100) 10)
    (swap! working-tileset conj (get-n-rand-tilecode-from-group 1 11)))
  (if (< (rand-int 100) 10)
    (swap! working-tileset conj (get-n-rand-tilecode-from-group 1 12)))
)

(defn random-tileset []
  (reset! working-tileset #{})
  (doseq [code (get-n-rand-tilecode-from-group 0 1) ]
      (swap! working-tileset conj code))
  (doseq [code (get-n-rand-tilecode-from-group 2 2) ]
      (swap! working-tileset conj code))
  (doseq [code (get-n-rand-tilecode-from-group 1 3) ]
      (swap! working-tileset conj code))
  (doseq [code (get-n-rand-tilecode-from-group 0 4) ]
      (swap! working-tileset conj code))
  (doseq [code (get-n-rand-tilecode-from-group 0 5) ]
      (swap! working-tileset conj code))
  (doseq [code (get-n-rand-tilecode-from-group 0 8) ]
      (swap! working-tileset conj code))
  (doseq [code (get-n-rand-tilecode-from-group 1 12) ]
      (swap! working-tileset conj code))
  (println "working tileset: " @working-tileset)
  )


(defn random-tileset-rand []
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


;(defn delete-nonconnected-tiles []
;  (doseq [tile (keys @tiles)]
;    ;(if (not (has-neighbours? tile))
;    (if (< (neighbour-count tile) @auto-delete-max-lonlieness)
;      (delete-tile tile))))


;(defn delete-fully-connected-tiles []
;  (let [fc-tiles (filter #(= (neighbour-count %1) 12) (keys @tiles))]
;    (doseq [tile fc-tiles]
;      (delete-tile tile))))


;(defn push-neighbours-todo [pos]
;  (dotimes [face 12]
;    (let [neighbour (get-neighbour pos face)]
;      (if (tileable? neighbour)
;          (push-todo neighbour)))))


;(defn push-connected-neighbours-todo [pos]
;  (doseq [idx (get-connected-idxs (@tiles pos))]
;    (let [neighbour (get-neighbour pos idx)]
;      (if (tileable? neighbour)
;          (push-todo neighbour)))))
;





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
  

(defn find-untilable-neighbours [pos tileset]
  (filter #(= 0 (count (find-candidates % tileset)))
          (get-neighbours pos)))


(defn choose-tilecode-old [pos tileset]
  (let [candidates (find-candidates pos tileset)]  
    (if (seq candidates)
      (nth candidates (rand-int (count candidates)))
      nil)))

; choose a tilecode for a position, but make sure that adding the
; tile doesnt create any untilable regions 
(defn choose-nonblocking-tilecode [pos tileset]
  (let [candidates (find-candidates pos tileset)
        nb-candidates (filter #() candidates)]
    (if (seq candidates)
      (nth candidates (rand-int (count candidates)))
      nil)))



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

 (defn push-connected-neighbours-to-empty-positions [pos]
  (doseq [idx (get-connected-idxs (@tiles pos))]
    (let [neighbour (get-neighbour pos idx)]
      (if (tileable? neighbour)
          (add-to-empty-positions neighbour)))))

 

(defn update-empty-positions-nonconnected []
  (do
    (init-empty-positions)
    (doseq [tile (keys @tiles)]
      (doseq [n (get-neighbours tile)]
        (if (tileable? n)
          (add-to-empty-positions n))))))


(defn update-empty-positions []
  (do
    (init-empty-positions)
    (doseq [pos (keys @tiles)]
      (push-connected-neighbours-to-empty-positions pos))))



(def dead-loci (atom #{}))

(defn init-dead-loci []
  (reset! dead-loci #{}))

(defn add-to-dead-loci [code]
  (do
    (swap! dead-loci conj code)
    ;(println "dead-loci:" @dead-loci)
    ))

;(defn creates-untilable-region? [pos]
;  (> (count (filter #(= 0 (count (find-candidates % @working-tileset)))
;                    (get-neighbours pos)))
;     0))


;(defn creates-untilable-region? [pos]
;  (> (count (filter #(contains? dead-loci (get-outer-facecode %))
;                     (get-neighbours pos))))
;     0)

(defn creates-untilable-region? [pos]
  (let [neighbours (get-neighbours pos)
        empty-neighbours (filter #(tileable? %) neighbours)
        untileable-neighbours (filter #(contains? @dead-loci 
                                                  (get-outer-facecode %))
                                      empty-neighbours)]
    (> (count untileable-neighbours) 0)))


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



(defn build-face-list []
  (reset! face-list #{})
  (doseq [tile (keys @tiles)]
    (add-tile-to-facelist tile)))


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
    (push-connected-neighbours-to-empty-positions pos)))

; _______________________________________________________________________


(defn init-tiler []
  (reset! tiles {})
  ;(init-todo)
  (init-empty-positions)
  (init-dead-loci)
  (seed-tiler)
  (reset! face-list #{}))


; _______________________________________________________________________



; this does facecode constrained tiling
;(defn make-tiling-iteration []
;  (if (and
;        (seq @todo)
;        (< (+ (count @todo) (count @tiles)) max-tiles ))
;    (let [new-pos (dequeue-todo)
;          new-code (choose-tilecode new-pos @working-tileset)]
;      (if (and (not= new-code "xxxxxxxxxxxx")
;               (= (count new-code) 12))
;        (do
;          (make-tile new-pos new-code)
;          (add-tile-to-facelist new-pos)
;          (push-connected-neighbours-todo new-pos))
;        (make-tile new-pos "xxxxxxxxxxxx")))))


; _______________________________________________________________________



; this does plain space filling with no facecode constraints
;(defn make-tiling-iteration-basic []
;  (if (seq @todo)
;    (let [new-pos (dequeue-todo)]
;      (make-tile new-pos "111111111111")
;      (push-neighbours-todo new-pos))))

(defn rotate-str-n [s n]
  (nth (rotations s) n))


;(defn update-assemblage-center []
;  (reset! assemblage-center (reduce vec3-add (keys @tiles))))


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
; fix rd-face to tilecode mapping so that opposite faces are 6 chars
; apart in the tilecode.
;
; Investigate and fix tilecode rotations, simple string rotation
; isn't correct as it is in 2d.
;
;




; _______________________________________________________________________


; returns a list of todo locations with 0 or 1 matching tiles
(defn find-best-positions []
  (filter #(< (count (find-candidates % @working-tileset)) 2)
          @empty-positions))

; returns a list of todo locations with any matching tiles
(defn find-any-positions []
  (filter #(> (count (find-candidates % @working-tileset)) 0)
          @empty-positions))

(defn choose-positions []
  (let [best (find-best-positions)]
    (if (= (count best) 0)
      (find-any-positions)
      best)))
  
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


(defn find-closest-to-point [positions point]
  (let [lengths (into {} 
                      (map #(vec [%1 (vec3-length (vec3-sub %1 point))])
                           positions))
        sorted-lengths (sort-by #(vec3-length (key %)) lengths)
        min-length ((first sorted-lengths) 1)
        tie-winners (filter #(= min-length (val %)) sorted-lengths) ]
    (if (= 1 (count tie-winners))
      ((first tie-winners) 0)
      ((rand-nth tie-winners) 0))))
; _______________________________________________________________________

(def autism 1.0)
(def adhd 2.0) ; lower = more adhd

(defn compute-backtrack-amount []
  (let [t (count @tiles)]
    (loop [n 1]
      (if (or (> n (- t 1))
              (> (rand)
                 (Math/pow (/ n (+ n autism)) adhd)))
        n
        (recur (inc n))))))

(defn backtrack []
  (let [num-tiles (count @tiles)
        n (compute-backtrack-amount)
        ni (- num-tiles n)]
    (when (and (> (count @tiles) 1)
               (< n num-tiles))

      ;(println "tilecount:" num-tiles ", backtracking" n "tiles")
      ; remove n most recent @tiles
      (reset! tiles (ordered-map (take ni @tiles)))
      ;(update-assemblage-center)
      (update-empty-positions)
      (println "| tiles:" num-tiles 
               "| backtracked:" n)
      (init-dead-loci)
      ;(build-face-list)
      )))
    

(defn make-backtracking-tiling-iteration []
  (when (and (< (count @tiles) max-tiles)
             (> (count @empty-positions) 0))
    (let [positions (choose-positions)]
      (if (> (count positions) 0)
        (let [assemblage-center (reduce vec3-add (keys @tiles))
        ;      new-pos (find-closest-to-point positions assemblage-center)
              new-pos (find-closest-to-center positions)
              new-code (choose-tilecode-old new-pos @working-tileset)]
          (if (not= new-code nil)
            (do
              (make-tile new-pos new-code)
              (let [untileable (find-untilable-neighbours new-pos
                                                          @working-tileset)]
                (if (> (count untileable) 0)
                ;(if (creates-untilable-region? new-pos)
                  (do
                    (doseq [u untileable]
                      (add-to-dead-loci (get-outer-facecode u)))
                    (delete-tile new-pos)
                    (backtrack)
                    
                    )
                  (do
                    (add-tile-to-facelist new-pos)
                    (push-connected-neighbours-to-empty-positions new-pos)
                    (remove-from-empty-positions new-pos)
                    ;(update-assemblage-center)
                  ))))
            (do
              (add-to-dead-loci (get-outer-facecode new-pos))
              ;(remove-from-empty-positions new-pos)
              (backtrack)
              )))
        (println "no tileable positions")
        ))))
        




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


