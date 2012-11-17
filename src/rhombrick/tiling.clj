(ns rhombrick.tiling
  (:use [rhombrick.vector]
        [rhombrick.staticgeometry :as geom]
        [rhombrick.facecode]
        [ordered.map]))

;(def tiler (atom {
;  :max-tiles 200
;  :state :halted
;  :iterations 0
;  :tiles (ordered-map)
;  :empty-positions #{}
;  :dead-loci #{}
;}))

; increment :iterations
; (swap! tiler assoc :iterations (inc (@tiler :iterations)))
;
; add a tile
; (swap! tiler assoc :tiles (assoc (@tiler :tiles) [0 0 0] "abc"))
;
;
;
;

(def max-tiles (atom 200))
(def tiles (atom (ordered-map)))
(def tiler-iterations (atom 0))
(def tiler-state (atom :halted)) ; :halted :running :paused
(def face-list (atom #{}))
(def assemblage-center (atom [0 0 0]))
(def assemblage-max-radius (atom 6))
(def dead-loci (atom #{}))
(def tileset-expanded (atom #{}))
(def facecode-compatible #{
  [\- \-]
  [\1 \1]
  [\2 \2]
  [\3 \3]
  [\4 \4]
  [\a \A]
  [\b \B]
  [\c \C]
  [\d \D]
  [\e \E]
  [\f \F]
                           })



; interesting tilesets:
;
; (set-current-tileset ...)
; #{"a01b01A01B01" "00000A00000a" "00000B00000b"})
; #{"000000001001" "000000001101" "a01b01A01B01" "00000A00000a" "00000B00000b"}
; #{"000000001001" "A00100a00100" "111111111111" "A00a00A00a00" "000001000001"
;   "00000a00000a" "A00000A00000"}
; #{"111111111111" "100000a00000" "A00000b00000" "B00000100000" }
; #{"000000010001" "A000a0001000"}
; ["A-AAA-A-AAA-" "-----A-----1" "-----a-----1" "-----1-----1"] 
; ["-----------1" "-A1--11aA---" "--------a---" "-1---------1" "-A------1-1-"]
;
; _______________________________________________________________________

(defn get-num-connected [code]
  (count (filter #(not= \- %) code)))


(defn make-random-tilecode []
  (let [digits [\- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
                \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
                \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
                \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
                \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
                \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
                \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1
                \2
                \3
                \a \a \a \a
                \A \A \A \A
                \b
                \B
                ]

        code (apply str (map (fn [_] (rand-nth digits)) (range 12)))]
    (if (= code "------------") "-----------1" code)))


(defn get-random-tileset []
  (let [num-tiles (+ 1 (rand-int 5))]
    (vec (map (fn [_] (make-random-tilecode))
         (range num-tiles)))))


; _______________________________________________________________________


(defn update-assemblage-center [new-pos]
  (let [new-center (vec3-scale (vec3-add new-pos @assemblage-center)
                               (/ 1.0 (count @tiles)))]
    (reset! assemblage-center new-center)))


(defn find-assemblage-center []
  (vec3-scale (reduce vec3-add (keys @tiles))
              (/ 1 (count @tiles))))


(defn get-neighbour-pos [pos face]
  (vec3-add pos (rd-neighbour-offsets face)))


(defn get-neighbours [pos]
  (vec (map #(get-neighbour-pos pos %) (range 12))))


(defn get-neighbourhood [pos]
  "returns a vector containing facecodes for all neighbours"
  (vec (map #(@tiles (get-neighbour-pos pos %)) (range 12))))



(defn is-empty? [pos]
  (not (contains? @tiles pos)))


(defn make-tile [pos facecode]
  (when (is-empty? pos)
    (swap! tiles assoc pos facecode)))


(defn delete-tile [pos]
  (swap! tiles dissoc pos))


(defn delete-neighbours [pos]
  (doseq [pos (get-neighbours pos)]
    (delete-tile [pos]) 
  ))

(defn neighbour-states [pos]
  "Returns vector of boolean, one for each neighbour. The value represents "
  "whether the abutting face is connected or not"
  (map #(not (is-empty? (vec3-add pos %1)))
       rd-neighbour-offsets))


(defn neighbour-count [pos]
  (count (filter #(true? %) (neighbour-states pos))))


(defn has-neighbours? [pos]
  (not (zero? (neighbour-count pos))))


(defn get-neighbour-abutting-face2 [neighbourhood face-idx]
  (let [op-face-idx (connecting-faces face-idx)
        nb-code (neighbourhood face-idx)]
    ;(println "nb-code:"nb-code "op-face-idx:" op-face-idx "neighbourhood:" neighbourhood)
    (if (nil? nb-code) \. (nth nb-code op-face-idx))))


;(defn get-outer-facecode [pos]
;  (apply str (map #(get-neighbour-abutting-face pos %) (range 12))))


(defn get-outer-facecode2 [neighbourhood]
  (apply str (map #(get-neighbour-abutting-face2 neighbourhood %) (range 12))))




; generate all unique rotations of tiles 
; intended to be be used on the working tileset
; when choosing a tile
;(defn expand-tiles [tiles]
;  (set (flatten (map #(rotations %) tiles))))


;(defn expand-tiles-experiment [tiles]
;  (set (flatten (conj (map #(rotations %) tiles)
;                      (map #(rotations %) (reverse-tiles tiles))))))

(defn expand-tiles-preserving-symmetry [tiles]
  (set (flatten (map #(get-code-symmetries %) tiles))))

;(defn expand-tiles-preserving-symmetry [tiles]
;  (set (flatten (conj (map #(rotations-preserving-symmetry %) tiles)
;                      (map #(rotations-preserving-symmetry %) (reverse-tiles tiles))))))





; compares single digits of two facecodes, using the
; compatibility table 
(defn face-digit-compatible? [inner outer]
  (or (contains? facecode-compatible [inner outer])
      (contains? facecode-compatible [outer inner])
      (= outer \.)))


(defn face-digit-like-compatible? [d]
  (not (contains? #{\a \b \c \d \e \f \A \B \C \D \E \F} d)))


; determine if faces are compatible without rotation
(defn facecodes-directly-compatible? [outercode innercode]
  (= 12 
     (count (filter #(true? %)
                    (map #(face-digit-compatible? %1 %2) 
                         innercode outercode)))))


;(defn find-candidates2 [neighbourhood tileset]
;  (let [outercode (get-outer-facecode2 neighbourhood)]
;    (if (contains? @dead-loci outercode)
;      ()
;      (filter #(facecodes-directly-compatible? outercode %)
;              (expand-tiles-preserving-symmetry tileset)))))

(defn find-candidates2 [neighbourhood tileset]
  (let [outercode (get-outer-facecode2 neighbourhood)]
    (if (contains? @dead-loci outercode)
      ()
      (filter #(facecodes-directly-compatible? outercode %)
              @tileset-expanded))))


; neighbourhood looks like ["000000001001" "000000001000" etc ]
(defn choose-tilecode2 [neighbourhood tileset]
  (let [candidates (find-candidates2 neighbourhood tileset)]  
    (if (seq candidates)
      (nth candidates (rand-int (count candidates)))
      nil)))



; _______________________________________________________________________




(def empty-positions (atom #{}))


(defn init-empty-positions []
  (reset! empty-positions #{}))


(defn add-to-empty-positions [pos]
  (if (and (< (+ (count @empty-positions) (count @tiles))
         @max-tiles)
           (< (vec3-length pos) @assemblage-max-radius))
    (swap! empty-positions conj pos)))


(defn remove-from-empty-positions [pos]
  (swap! empty-positions disj pos))


;(defn push-neighbours-to-empty-positions [pos]
;  (dotimes [face 12]
;    (let [neighbour (get-neighbour-pos pos face)]
;      (if (is-empty? neighbour)
;          (add-to-empty-positions neighbour)))))


 (defn push-connected-neighbours-to-empty-positions [pos]
  (doseq [idx (get-connected-idxs (@tiles pos))]
    (let [neighbour (get-neighbour-pos pos idx)]
      (if (is-empty? neighbour)
          (add-to-empty-positions neighbour)))))
 

;(defn update-empty-positions-nonconnected []
;  (do
;    (init-empty-positions)
;    (doseq [tile (keys @tiles)]
;      (doseq [n (get-neighbours tile)]
;        (if (is-empty? n)
;          (add-to-empty-positions n))))))



(defn update-empty-positions []
  (do
    (init-empty-positions)
    (doseq [pos (keys @tiles)]
      (push-connected-neighbours-to-empty-positions pos))))





(defn init-dead-loci []
  (reset! dead-loci #{}))


(defn add-to-dead-loci [code]
  (do
    (swap! dead-loci conj code)
    ;(println "dead-loci:" @dead-loci)
    ))



(defn creates-untilable-region-orig? [pos]
  (let [neighbours (get-neighbours pos)
        empty-neighbours (filter #(is-empty? %) neighbours)
        untileable-neighbours (filter #(contains? @dead-loci 
                                                  (get-outer-facecode2 (get-neighbourhood %)))
                                      empty-neighbours)]
    (> (count untileable-neighbours) 0)))


(defn get-untileable-neighbours [pos]
  (->> (get-neighbours pos)
       (filter #(is-empty? %))
       (filter #(contains? @dead-loci 
                           (get-outer-facecode2 (get-neighbourhood %))))))

(defn creates-untileable-region? [pos]
  (> (count (->> (get-neighbours pos)
                 (filter #(is-empty? %))
                 (filter #(contains? @dead-loci 
                                     (get-outer-facecode2 (get-neighbourhood %))))))
     0))


;(defn creates-untilable-region2? [pos code]
;  (let [neighbours (get-neighbours pos)
;        neighbours-new (assoc neighbours (connecting-faces
;        empty-neighbours (filter #(is-empty? %) neighbours)
;        en-outer-facecodes (map #(get-outer-facecode2 (get-neighbourhood %)) empty-neighbours)
;        ;untileables (filter #(contains? @dead-loci (get-outer-facecode2 %))
;        ;                    empty-neighbours)
;        ]
;    (> (count (filter #(= 0 (count %))
;                      (map #(find-candidates2 (get-neighbourhood %) #{code})
;                           empty-neighbours)))
;       0)))


;(defn creates-untilable-region2? [_] false)

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

(defn update-tileset-expanded [tileset]
  (when (> (count tileset) 0)
    (reset! tileset-expanded
            (expand-tiles-preserving-symmetry tileset))))


(defn seed-tiler [tileset]
  (when (> (count tileset) 0)
  (let [pos [0 0 0]
        code (rand-nth (vec (expand-tiles-preserving-symmetry tileset)))]
    (make-tile pos code) 
    (push-connected-neighbours-to-empty-positions pos))))

; _______________________________________________________________________


(defn init-tiler [tileset]
  (reset! tiles {})
  (reset! tiler-iterations 0)
  (reset! face-list #{})
  (update-tileset-expanded tileset)
  (init-empty-positions)
  (init-dead-loci)
  (seed-tiler tileset)
  (reset! tiler-state :running)
  (println "tiler started"))

; same as init-tiles but doesnt reset dead-loci
(defn soft-init-tiler [tileset]
  (reset! tiles {})
  (reset! tiler-iterations 0)
  (reset! face-list #{})
  (update-tileset-expanded tileset)
  (init-empty-positions)
  (seed-tiler tileset)
  (reset! tiler-state :running)
  (println "tiler started"))



(defn halt-tiler []
  (reset! tiler-state :halted)
  (println "#  HALTED - "
           "tiles:" (count @tiles)
           "iters:" @tiler-iterations
           "dead:" (count @dead-loci)
           "i/t" (float (/ @tiler-iterations (count @tiles)))
           "t/i" (float (/ (count @tiles) @tiler-iterations ))
           ;"tileset:" @rhombrick.editor/current-tileset))
           ))

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



; I think the less perfect patterns encountered along the way would be
; interesting in their own right, however. I encourage you to look at some of
; them. The perfected patterns are neat and one can imagine deliberately
; engineering them by mimicking your procedure. But the imperfect ones that
; would be created by bounded-scale backtracking would also be interesting.
; Try treating the backtracking as a model parameter, and see what sorts of
; patterns you get with none, with backtracking only a single step, with
; backtracking 1-3 steps with a damped distribution (something as simple
; as {1,1,1,1,2,2,3} would work fine), and compare the result to scale free
; backtracking.
;
; Programmatically, put in a modular subroutine at that step and let it depend
; on a global parameter. Then do runs - from this set of allowed tiles, here
; are some shapes that arise with no backtracking, with backtracking 1, with
; backtracking 1-3 damped, with scale free backtracking. I am sure the results
; would be interesting. The point generalizes. To understand the impact of a
; given step in an algorithm on the overall behaviors seen, one does
; "sensitivity analysis" on that step.
;
; [http://forum.wolframscience.com/showthread.php?s=&threadid=866]

; _______________________________________________________________________


; returns a list of todo locations with 0 or 1 matching tiles
;(defn find-best-positions [tileset]
;  (filter #(< (count (find-candidates % tileset)) 2)
;          @empty-positions))

(defn find-best-positions2 [tileset]
  (filter #(< (count (find-candidates2 (get-neighbourhood %) tileset)) 2)
          @empty-positions))


; returns a list of todo locations with any matching tiles
;(defn find-any-positions [tileset]
;  (filter #(> (count (find-candidates % tileset)) 0)
;          @empty-positions))


(defn find-any-positions2 [tileset]
  (filter #(> (count (find-candidates2 (get-neighbourhood %) tileset)) 0)
          @empty-positions))


(defn choose-positions [tileset]
  (let [best (find-best-positions2 tileset)]
    (if (= (count best) 0)
      (find-any-positions2 tileset)
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
      (reset! assemblage-center (find-assemblage-center))
      (update-empty-positions)
      ;(println "| tiles:" num-tiles 
      ;         "| backtracked:" n)
      (build-face-list)
      )))
    

(defn make-backtracking-tiling-iteration2 [tiles tileset]
  (if-let [positions (choose-positions tileset)]
    (let [new-pos (find-closest-to-point positions (find-assemblage-center))
          new-neighbourhood (get-neighbourhood new-pos)
          new-code (choose-tilecode2 new-neighbourhood tileset)]
      (if (nil? new-code)
        (do
          (add-to-dead-loci (get-outer-facecode2 new-neighbourhood))
          (delete-neighbours new-pos))
        (do
          (make-tile new-pos new-code)
          (reset! assemblage-center (find-assemblage-center))))
      (if (or (creates-untileable-region? new-pos)
              (nil? new-code))
        (do
          (delete-tile new-pos)
          (delete-neighbours new-pos)
          (backtrack))
        (do
          (add-tile-to-facelist new-pos)
          (push-connected-neighbours-to-empty-positions new-pos)
          (remove-from-empty-positions new-pos)))
      (swap! tiler-iterations inc))
    (halt-tiler)))

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



; map tilecodes between pfh's 2d codes and the current implementation
;
; hexagonal:
;   "AaAa--"  ->  "Aa-----A-a--"
;
;   hex index map:
;   [0 1 3 6 7 9]
;
; square:
; ...

(def pfh-tilecode-map-hex [0 1 3 6 7 9 ])

(defn convert-pfh-tilecode-hex [phf-code]
  (apply str (map #(if (some #{%} pfh-tilecode-map-hex)
                    (.charAt phf-code (.indexOf pfh-tilecode-map-hex %))
                    \-)
                  (range 12))))


; pfh code for CA rule 110: 
(def ca-rule-110 [
                  "a-aC-C"
                  "a-bC-D"
                  "b-aD-C"
                  "b-bD-D"
                  "cacAAA" ; 0
                  "dacBBB" ; 1
                  "cbcBBB" ; 1
                  "dbcBBB" ; 1
                  "cadAAA" ; 0
                  "dadBBB" ; 1
                  "cbdBBB" ; 1
                  "dbdAAA" ; 0
                  ])

