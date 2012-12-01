(ns rhombrick.tiling
  (:use [rhombrick.vector]
        [rhombrick.staticgeometry :as geom]
        [rhombrick.facecode]
        [ordered.map]))


(def max-tiles (atom 1000))
(def tiles (atom (ordered-map)))
(def tiler-iterations (atom 0))
(def tiler-state (atom :halted)) ; :halted :running :paused
(def assemblage-center (atom [0 0 0]))
(def assemblage-max-radius (atom 20))
(def dead-loci (atom #{}))
(def tiler-thread (atom nil))
(def tiler-thread-id (atom 0))
(def last-iteration-time (atom 0))


(def facecode-compatible #{
  [\- \-]
  [\0 \0]
  [\1 \1]
  [\2 \2]
  [\3 \3]
  [\4 \4]
  [\5 \5]
  [\6 \6]
  [\7 \7]
  [\8 \8]
  [\9 \9]
  [\a \A]
  [\b \B]
  [\c \C]
  [\d \D]
  [\e \E]
  [\f \F] })


; NOTE: the tile renderer currently expects face digits no higher than 4 or D 
; for simplicity, even though the tiler will handle any hex digit.
(def ^:const random-tilecode-distribution [
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \0 \0 \0 \0 \0 \0 \0 \0 \0 \0 \0 \0 \0 \0 \0 \0
  \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1
  \2 \2 \2 \2 \2 \2 \2 \2
  \3 \3 \3 \3
  \4 \4 \4 \4
  \5 \5 \5 \5
  \6 \6 \6 \6
  \a \a \a \a \a \a \a \a
  \A \A \A \A \A \A \A \A
  \b \b \b \b
  \B \B \B \B
  \c \c \c \c
  \C \C \C \C
  \d \d \d \d
  \D \D \D \D])

(defn make-random-tilecode []
  (let [code (apply str (map (fn [_] (rand-nth random-tilecode-distribution))
                             (range 12)))]
    (if (= code "------------")
      (make-random-tilecode) 
      code)))


(defn get-random-tileset []
  (let [num-tiles (+ 1 (rand-int 10))]
    (vec (map (fn [_] (make-random-tilecode))
         (range num-tiles)))))


;(defn update-assemblage-center [new-pos]
;  (let [new-center (vec3-scale (vec3-add new-pos @assemblage-center)
;                               (/ 1.0 (count @tiles)))]
;    (reset! assemblage-center new-center)))


(defn get-num-connected [code]
  (count (filter #(not= \- %) code)))


(defn find-assemblage-center [_tiles]
  (if (> (count _tiles) 0)
    (vec3-scale (reduce vec3-add (keys _tiles))
                (/ 1 (count _tiles)))
    [0 0 0]))


(defn is-empty? [_tiles pos]
  (not (contains? _tiles pos)))


(defn get-neighbour-pos [pos face]
  (vec3-add pos (rd-neighbour-offsets face)))


(defn get-neighbours [pos]
  "returns a vector containing the positions of the neighbours of a given point"
  (vec (map #(get-neighbour-pos pos %) (range 12))))


(defn get-neighbourhood [_tiles pos]
  "returns a vector containing facecodes for all neighbours"
  (vec (map #(_tiles (get-neighbour-pos pos %)) (range 12))))


(defn make-tile! [pos facecode]
  (when (is-empty? @tiles pos)
    (swap! tiles assoc pos facecode)))


(defn make-tile [_tiles pos facecode]
  (assoc _tiles pos facecode))


(defn delete-tile [_tiles pos]
  (dissoc _tiles pos))


(defn delete-neighbours [_tiles pos]
;  (->> _tiles
;       (filter #(not (contains? (get-neighbours pos) %)))
;       (filter #(not (contains? pos)))))
  (filter #(not (contains? (get-neighbours pos) %)) _tiles))


(defn get-neighbour-abutting-face2 [neighbourhood face-idx]
  (let [op-face-idx (connecting-faces face-idx)
        nb-code (neighbourhood face-idx)]
    ;(println "nb-code:"nb-code "op-face-idx:" op-face-idx "neighbourhood:" neighbourhood)
    (if (nil? nb-code) \. (nth nb-code op-face-idx))))


(defn get-outer-facecode2 [neighbourhood]
  (apply str (map #(get-neighbour-abutting-face2 neighbourhood %) (range 12))))


; generate all unique rotations of tiles in tileset 
(defn expand-tiles-preserving-symmetry [tiles]
  (set (flatten (map #(get-code-symmetries %) tiles))))


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


(defn find-candidates2 [neighbourhood tileset]
  (let [outercode (get-outer-facecode2 neighbourhood)]
    (if (contains? @dead-loci outercode)
      ()
      (filter #(facecodes-directly-compatible? outercode %)
              tileset))))


; neighbourhood looks like ["000000001001" "000000001000" etc ]
(defn choose-tilecode2 [neighbourhood tileset]
  (let [candidates (find-candidates2 neighbourhood tileset)]  
    (if (seq candidates)
      (nth candidates (rand-int (count candidates)))
      nil)))


(defn get-connected-neighbours [_tiles pos]
  (if (contains? _tiles pos)
    (map #(get-neighbour-pos pos %) (get-connected-idxs (_tiles pos)))
    ()))


(defn get-empty-connected-neighbours [_tiles pos]
  (->> (get-connected-neighbours _tiles pos)
       (filter #(is-empty? _tiles %))
       (filter #(< (vec3-length %) @assemblage-max-radius))))

;(defn get-empty-neighbours [_tiles pos]
;  (->> (get-connected-idxs (_tiles pos))
;       (map #(get-neighbour-pos pos %))
;       (filter is-empty?)
;       (filter #(< (vec3-length %) @assemblage-max-radius))
;    ))



;(defn get-empty-positions [_tiles]
;  (into #{} (apply concat (map #(get-empty-connected-neighbours _tiles %) (keys _tiles)))))

(defn get-empty-positions [_tiles]
  (if (= (count _tiles) 0)
    #{[0 0 0]}
    (->> (keys _tiles)
         (map #(get-empty-connected-neighbours _tiles %))
         (apply concat)
         (set))))


(defn init-dead-loci! []
  (reset! dead-loci #{}))


(defn add-to-dead-loci! [code]
  (do
    (swap! dead-loci conj code)
    ;(println "dead-loci:" @dead-loci)
    ))


(defn creates-untileable-region? [_tiles pos]
  (> (count (->> (get-connected-neighbours _tiles pos)
                 (filter #(is-empty? _tiles %))
                 (filter #(contains? @dead-loci 
                                     (get-outer-facecode2 (get-neighbourhood _tiles %))))))
     0))


; returns a list of todo locations with 0 or 1 matching tiles
(defn find-best-positions2 [_tiles tileset empty-positions]
  (filter #(< (count (find-candidates2 (get-neighbourhood _tiles %) tileset)) 2)
          empty-positions))


; returns a list of todo locations with any matching tiles
(defn find-any-positions2 [_tiles tileset empty-positions]
  (filter #(> (count (find-candidates2 (get-neighbourhood _tiles %) tileset)) 0)
          empty-positions))


(defn choose-positions [_tiles tileset empty-positions]
  (let [best (find-best-positions2 _tiles tileset empty-positions)]
    (if (= (count best) 0)
      (find-any-positions2 _tiles tileset empty-positions)
      best)))
 


; Receive a vector of positions and return the closest to the center
; ie the vector with the shortest length. If there are more than one
; with length equal to the shortest length then return a random one.
(defn find-closest-to-center [positions]
  (let [sorted (->> (map #(vec [%1 (vec3-sum-of-squares %1)]) positions)
                    (sort-by #(% 1)))
        min-length ((first sorted) 1)
        tie-winners (filter #(= min-length (% 1)) sorted)]
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


(def autism (atom 1.0))
(def adhd (atom 2.0)) ; lower = more adhd

;(def ^const stats-buffer-length 1000)
;(def stats-tile-count (atom []))
;(def stats-empty-count (atom []))
;(def stats-dead-count (atom []))
;(def stats-backtrack (atom []))
;(def stats-iter-time (atom []))
;(defn update-stats-buffer []
;  )

(defn compute-backtrack-amount [num-tiles]
  (loop [n 1]
    (if (or (>= n num-tiles)
            (> (rand) (Math/pow (/ n (+ n @autism)) @adhd)))
      n
      (recur (inc n)))))


(defn backtrack [_tiles]
  (let [num-tiles (count _tiles)
        n (compute-backtrack-amount num-tiles)
        ni (- num-tiles n)]
    (if (and (> num-tiles 0)
             (<= n num-tiles))
      (take ni _tiles)
      _tiles)))


(defn make-backtracking-tiling-iteration3 [_tiles tileset]
  (if-let [positions (choose-positions _tiles tileset (get-empty-positions _tiles))]
    (let [new-pos (find-closest-to-center positions)
          ;new-pos (find-closest-to-point positions [0 0 0])
          ;new-pos (find-closest-to-point positions (find-assemblage-center _tiles))
          new-neighbourhood (get-neighbourhood _tiles new-pos)
          new-code (choose-tilecode2 new-neighbourhood tileset)]
      (if (nil? new-code)
        (do
          ;(println "no tile would fit")
          (add-to-dead-loci! (get-outer-facecode2 new-neighbourhood))
          (->> (delete-neighbours _tiles new-pos)
               (backtrack))
          )
        (let [new-tiles (make-tile _tiles new-pos new-code)]
          (if (creates-untileable-region? new-tiles new-pos)
            (do
              ;(println "dead end, backtracking")
              (backtrack _tiles)
              )
            new-tiles))))
    (do
      (reset! tiler-state :halted)
      _tiles)))


(defn halt-tiler []
  (reset! tiler-state :halted)
  (println "tiler-state -> halted"))


(defn tiler-can-iterate? []
  (and (= @tiler-state :running)
       (> (count @tiles) 0)
       (< (count @tiles) @max-tiles)
       (> (count (get-empty-positions @tiles)) 0)
       ))


(defn run-backtracking-tiling-thread [_tiles tileset]
  (println "tiler thread starting")
  (let [tileset-expanded (expand-tiles-preserving-symmetry tileset)]
    ;(while tiler-can-iterate? 
    (while (and (= @tiler-state :running)
                ;(> (count @tiles) 0)
                (< (count @tiles) @max-tiles)
                (> (count (get-empty-positions @tiles)) 0)
                )
      (let [iter-start-time (System/nanoTime)]
        (dosync
          (reset! tiles (ordered-map (make-backtracking-tiling-iteration3 @tiles tileset-expanded)))
          (swap! tiler-iterations inc))
          (reset! last-iteration-time (/ 1000 (float (/ (- (System/nanoTime) iter-start-time) 1000000.0))))
        )))
  (halt-tiler))


(defn seed-tiler [tileset]
  (when (> (count tileset) 0)
  (let [pos [0 0 0]
        code (rand-nth (vec (expand-tiles-preserving-symmetry tileset)))]
    (make-tile! pos code) 
    )))


(defn cancel-tiler-thread []
  (when (future? @tiler-thread)
    (future-cancel @tiler-thread)
    (if (or (future-cancelled? @tiler-thread)
            (future-done? @tiler-thread))
      (halt-tiler)
      (println "cancel-tiler-thread failed"))))


(defn init-tiler [tileset]
  (reset! tiles (ordered-map))
  (reset! tiler-iterations 0)
  )


(defn start-tiler [tileset soft-start?]
  (cancel-tiler-thread)
  (Thread/sleep 100)
  (init-tiler tileset)
  (seed-tiler tileset)
  (when-not soft-start?
    (init-dead-loci!))
  (reset! tiler-state :running)
  (reset! tiler-thread (future (run-backtracking-tiling-thread @tiles tileset))))



; _______________________________________________________________________



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

; _______________________________________________________________________

