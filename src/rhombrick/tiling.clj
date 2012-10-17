(ns rhombrick.tiling
  (:use [rhombrick.vector]
        [rhombrick.staticgeometry :as geom]
        [rhombrick.facecode]
        [ordered.map]))


(def max-tiles (atom 200))
;(def tiles (atom {}))
(def tiles (atom (ordered-map)))

(def tiler-iterations (atom 0))

(def tiler-state (atom :halted)) 

(def facecode-compatible #{
  [\0 \0]
  [\1 \1]
  [\a \A]
  [\b \B]
  [\c \C]
  [\d \D]
  [\e \E]
  [\f \F]
                           })

(def face-list (atom #{}))

(def assemblage-center (atom [0 0 0]))


; interesting tilesets:
;
; (set-current-tileset ...)
; #{"a01b01A01B01" "00000A00000a" "00000B00000b"})
; #{"000000001001" "000000001101" "a01b01A01B01" "00000A00000a" "00000B00000b"}
; #{"000000001001" "A00100a00100" "111111111111" "A00a00A00a00" "000001000001"
;   "00000a00000a" "A00000A00000"}


; _______________________________________________________________________

(defn get-num-connected [code]
  (count (filter #(and (not= \0 %) (not= \- %)) code)))


(defn get-n-rand-tilecode [n]
  (vec 
    (map (fn [a] (rand-nth (take 352 (seq @normalised-facecodes-sorted))))
         (range n))))


(defn get-n-rand-tilecode-from-group [n g]
  (map (fn [a] (rand-nth (@normalised-facecodes-grouped g)))
         (range n)))


(defn make-random-tilecode []
  (let [digits [\0 \0 \0 \0 \0 \0 \0 \0 \0 \1 \a \A]]
    (apply str (map (fn [_] (rand-nth digits))
                    (range 12)))))

(defn get-random-tileset []
  (let [num-tiles (+ 1 (rand-int 5))]
    (set (map (fn [_] (make-random-tilecode))
         (range num-tiles)))))


; _______________________________________________________________________


(defn update-assemblage-center [new-pos]
  (let [new-center (vec3-scale (vec3-add new-pos @assemblage-center)
                               (/ 1 (count @tiles)))]
    (reset! assemblage-center new-center)))


(defn find-assemblage-center []
  (vec3-scale (reduce vec3-add (keys @tiles))
              (/ 1 (count @tiles))))


(defn get-neighbour [pos face]
  (vec3-add pos (rd-neighbour-offsets face)))


(defn get-neighbours [pos]
  (vec (map #(get-neighbour pos %) (range 12))))


(defn tileable? [pos]
  (not (contains? @tiles pos)))


(defn make-tile [pos facecode]
  (when (tileable? pos)
    (swap! tiles assoc pos facecode)))


(defn delete-tile [pos]
  (swap! tiles dissoc pos))


(defn neighbour-states [pos]
  (map #(not (tileable? (vec3-add pos %1)))
       rd-neighbour-offsets))


(defn neighbour-count [pos]
  (count (filter #(true? %) (neighbour-states pos))))


(defn has-neighbours? [pos]
  (not (zero? (neighbour-count pos))))


(defn get-neighbour-abutting-face [pos face]
    (let [op-face (connecting-faces face)
          nb-pos (get-neighbour pos face)
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


(defn expand-tiles-experiment [tiles]
  (set (flatten (map #(rotations %)
                     (conj tiles (vec (map reverse tiles)))))))



; compares single digits of two facecodes, using the
; compatibility table 
(defn face-digit-compatible? [a b]
  (or (contains? facecode-compatible [a b])
      (contains? facecode-compatible [b a])
      (= a \-)
      (= b \-)))


(defn face-digit-like-compatible? [d]
  (not (contains? #{\a \b \c \d \e \f \A \B \C \D \E \F} d)))

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
  
(defn choose-tilecode [pos tileset]
  (let [candidates (find-candidates pos tileset)]  
    (if (seq candidates)
      (nth candidates (rand-int (count candidates)))
      nil)))



; _______________________________________________________________________




(def empty-positions (atom #{}))


(defn init-empty-positions []
  (reset! empty-positions #{}))


(defn add-to-empty-positions [pos]
  (if (< (+ (count @empty-positions)
            (count @tiles))
         @max-tiles)
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
 

;(defn update-empty-positions-nonconnected []
;  (do
;    (init-empty-positions)
;    (doseq [tile (keys @tiles)]
;      (doseq [n (get-neighbours tile)]
;        (if (tileable? n)
;          (add-to-empty-positions n))))))



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
    (println "dead-loci:" @dead-loci)
    ))



(defn creates-untilable-region? [pos]
  (let [neighbours (get-neighbours pos)
        empty-neighbours (filter #(tileable? %) neighbours)
        untileable-neighbours (filter #(contains? @dead-loci 
                                                  (get-outer-facecode %))
                                      empty-neighbours)]
    (> (count untileable-neighbours) 0)))


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


(defn add-tile-to-facelist2 [pos]
  (let [code (@tiles pos)
        idxs (get-nonconnected-idxs code)
        faces (map #(rd-faces %1) idxs)]
    (doseq [f faces]
      (let [fv (face-idxs-to-verts f)
            fvw (vec (map #(vec3-add pos (vec3-scale % 0.5)) fv))]
        ;(swap! face-list conj fvw)))))
        (if (not (facelist-contains-rotations? fvw))
          (swap! face-list conj fvw))))))
        ; (remove-from-facelist fvw))))))


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


(defn seed-tiler [tileset]
  (let [pos [0 0 0]
        code (choose-tilecode pos tileset)]
    (make-tile pos code) 
    (push-connected-neighbours-to-empty-positions pos)))

; _______________________________________________________________________


(defn init-tiler [tileset]
  (reset! tiles {})
  (reset! tiler-iterations 0)
  (reset! face-list #{})
  (init-empty-positions)
  (if (not= @rhombrick.editor/current-tileset tileset)
    (init-dead-loci))
  (seed-tiler tileset)
  (reset! tiler-state :running)
  (println "tiler started"))


(defn halt-tiler []
  (reset! tiler-state :halted)
  (println "tiler halted"))
; _______________________________________________________________________


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
; fix rd-face to tilecode mapping so that opposite faces are 6 chars
; apart in the tilecode.
;
; Investigate and fix tilecode rotations, simple string rotation
; isn't correct as it is in 2d.
;
;




; _______________________________________________________________________


; returns a list of todo locations with 0 or 1 matching tiles
(defn find-best-positions [tileset]
  (filter #(< (count (find-candidates % tileset)) 2)
          @empty-positions))


; returns a list of todo locations with any matching tiles
(defn find-any-positions [tileset]
  (filter #(> (count (find-candidates % tileset)) 0)
          @empty-positions))


(defn choose-positions [tileset]
  (let [best (find-best-positions tileset)]
    (if (= (count best) 0)
      (find-any-positions tileset)
      best)))
 

; _______________________________________________________________________

; Receive a vector of positions and return the closest to the center
; ie the vector with the shortest length. If there are more than one
; with length equal to the shortest length then return a random one.
;
; !FIXME! These next two shouldnt need to call vec3-length twice. In
; fact since we dont need the actual length, only compare them to find
; the shortest, we shouldnt use the length function just sum of squares
; so avoid calling sqrt!
;
; It is worth optimising because it's called at least once per cell.

(defn find-closest-to-center [positions]
  (let [lengths (into {} (map #(vec [%1 (vec3-length %1)]) positions))
        sorted-lengths (sort-by #(vec3-length (key %)) lengths)
        min-length ((first sorted-lengths) 1)
        tie-winners (filter #(= min-length (val %)) sorted-lengths) ]
    (if (= 1 (count tie-winners))
      ((first tie-winners) 0)
      ((rand-nth tie-winners) 0))))


(defn find-closest-to-point [positions point]
  (let [lengths (into {} (map #(vec [%1 (vec3-sum-of-squares (vec3-sub %1 point))])
                              positions))
        sorted-lengths (sort-by #(vec3-sum-of-squares (key %)) lengths)
        min-length ((first sorted-lengths) 1)
        tie-winners (filter #(= min-length (val %)) sorted-lengths) ]
    (if (= 1 (count tie-winners))
      ((first tie-winners) 0)
      ((rand-nth tie-winners) 0))))

; _______________________________________________________________________

(def autism (atom 1.0))
(def adhd (atom 2.0)) ; lower = more adhd


(defn compute-backtrack-amount [num-tiles]
  (let [t num-tiles]
    (loop [n 1]
      (if (or (> n (- t 1))
              (> (rand)
                 (Math/pow (/ n (+ n @autism)) @adhd)))
        n
        (recur (inc n))))))


(defn backtrack []
  (let [num-tiles (count @tiles)
        n (compute-backtrack-amount num-tiles)
        ni (- num-tiles n)]
    (when (and (> num-tiles 1)
               (< n num-tiles))
      ; remove n most recent @tiles
      (reset! tiles (ordered-map (take ni @tiles)))
      ;(update-assemblage-center)
      (update-empty-positions)
      ;(println "| tiles:" num-tiles 
      ;         "| backtracked:" n)
      (build-face-list)
      )))
    

(defn make-backtracking-tiling-iteration [tileset]
  (when (= @tiler-state :running)
    (if (and (< (count @tiles) @max-tiles)
             (> (count @empty-positions) 0)
             (> (count tileset) 0))
      (when-let [positions (choose-positions tileset)]
        (let [new-pos (find-closest-to-point positions (find-assemblage-center))
              new-code (choose-tilecode new-pos tileset)]
          (if (nil? new-code)
            (add-to-dead-loci (get-outer-facecode new-pos))
            (make-tile new-pos new-code))
          (if (or (creates-untilable-region? new-pos)
                  (nil? new-code))
            (do
              (delete-tile new-pos)
              (backtrack))
            (do
              (add-tile-to-facelist new-pos)
              (push-connected-neighbours-to-empty-positions new-pos)
              (remove-from-empty-positions new-pos))))
        (swap! tiler-iterations inc))
      (halt-tiler))))
       

; _______________________________________________________________________



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


