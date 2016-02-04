(ns rhombrick.tiling
  (:use [rhombrick.tilecode]
        [rhombrick.vector]
        [rhombrick.staticgeometry]
        [ordered.map]
        ;[overtone.osc]
        ))


(def assemblage-center (atom [0 0 0]))
(def last-iteration-time (atom 0))

(def sonify? false)
;(when sonify?
;  (def OSCPORT2 4242)
;  (def client2 (osc-client "localhost" OSCPORT2)))

;(defn update-assemblage-center [new-pos]
;  (let [new-center (vec3-scale (vec3-add new-pos @assemblage-center)
;                               (/ 1.0 (count @tiles)))]
;    (reset! assemblage-center new-center)))


(defn find-assemblage-center [tiles]
  (if (> (count tiles) 0)
    (vec3-scale (reduce vec3-add (keys tiles))
                (/ 1 (count tiles)))
    [0 0 0]))


(defn update-assemblage-center [tiles]
  (reset! assemblage-center (find-assemblage-center tiles)))


(defn is-empty? [tiles pos]
  (not (contains? tiles pos)))

(defn quantize-position [v]
  (cond
    (= (@current-topology :id) :hexagon)
      (vec3-quantize v 6)
    (= (@current-topology :id) :hexagonal-prism)
      (vec3-quantize v 6)
    :else
      v) )

(defn get-neighbour-pos [pos face]
  (if (or (= (@current-topology :id) :hexagon)
          (= (@current-topology :id) :square))
    (quantize-position
      (vec3-add pos
                (vec3-scale ((@current-topology :face-centers-ideal) face) 2 ))))
    (quantize-position
      (vec3-add pos
                (vec3-scale ((@current-topology :face-centers) face) 2 ))))




  ;(vec3-add pos ((@current-topology :face-centers) face)))


(defn get-neighbours [pos]
  "returns a vector containing the positions of the neighbours of a given point"
  (vec (map #(get-neighbour-pos pos %) (range (@current-topology :num-faces)))))


(defn get-neighbourhood [tiles pos]
  "returns a vector containing facecodes for all neighbours"
  (vec (map #(tiles (get-neighbour-pos pos %)) (range (@current-topology :num-faces)))))


(defn get-connected-neighbours [tiles pos]
  (if (contains? tiles pos)
    (map #(get-neighbour-pos pos %) (get-connected-idxs (tiles pos)))
    ()))


(defn get-empty-connected-neighbours [tiles pos]
  (->> (get-connected-neighbours tiles pos)
       (filter #(is-empty? tiles %))
       ;(filter #(< (vec3-length %) @assemblage-max-radius))
    ))

;(defn get-occupied-neighbours [tiles pos]
;  (->> (get-neighbours tiles pos)
;       (filter #(not (is-empty? tiles %)))
;       ;(filter #(< (vec3-length %) @assemblage-max-radius))
;    ))

(defn get-occupied-connected-neighbours [tiles pos]
  (->> (get-connected-neighbours tiles pos)
       (filter #(not (is-empty? tiles %)))
       ;(filter #(< (vec3-length %) @assemblage-max-radius))
    ))

(defn neighbour-connects-to-this? [tiles pos nb-idx]
  (let [nb-digit (get-neighbour-abutting-face2 (get-neighbourhood tiles pos) nb-idx)]
    (and
      (not= nb-digit \-)
      (not= nb-digit \.))))


(defn make-tile [ts pos facecode]
  ;(when sonify?
  ;  (osc-send client2 "/rhombrick.tiling" "make-tile" (int (mod (tilecode-to-number facecode) 21))))
  (assoc ts :tiles (assoc (ts :tiles) (quantize-position pos) facecode)))


(defn get-empty-neighbours [tiles pos]
  (->> (get-neighbours pos)
       (filter #(is-empty? tiles %))))


(defn make-tile-2 [ts pos facecode]
  (let [qpos (quantize-position pos)
        new-tiles (assoc (ts :tiles) qpos facecode)
        max-rad (get-in ts [:params :max-radius])
        empty-cn (if (get-in ts [:params :nfc?])
                   (set (get-empty-neighbours new-tiles qpos))
                   (get-empty-connected-neighbours new-tiles qpos))
        empty-cn-within-rad (set (filter #(< (vec3-length %) max-rad) empty-cn))
        tmp-empty (disj (ts :empty) qpos)
        new-empty (clojure.set/union tmp-empty empty-cn-within-rad)
        ]
    ;(when sonify?
    ;  (osc-send client2
    ;            "/rhombrick.tiling" "make-tile"
    ;            (int (mod (tilecode-to-number facecode) 21))))
    (-> ts
      (assoc :tiles (ordered-map new-tiles))
      (assoc :empty new-empty))))


(defn delete-neighbours [tiles pos]
  (ordered-map (filter #(not (contains? (get-neighbours pos) %)) tiles)))


(defn delete-neighbours-ts [ts pos]
  (assoc ts :tiles (delete-neighbours (ts :tiles) pos)))



;(defn get-empty-neighbours [tiles pos]
;  (->> (get-connected-idxs (tiles pos))
;       (map #(get-neighbour-pos pos %))
;       (filter is-empty?)
;       (filter #(< (vec3-length %) @assemblage-max-radius))
;    ))

; true = yes, valid empty position
; false = not empty
(defn get-empty-status [tiles pos]
  (and
    (not (contains? tiles pos))
    (->> (range (@current-topology :num-faces))
         (map #(neighbour-connects-to-this? tiles pos %))
         (filter true?)
         (count)
         (not= 0)
         )))

(defn get-empty-status-nfc [tiles pos]
  (and
    (not (contains? tiles pos))
    (->> (range (@current-topology :num-faces))
         (map #(not (is-empty? tiles (get-neighbour-pos pos %))))
         (filter true?)
         (count)
         (not= 0)
         )))


(defn get-empty-positions [tiles max-radius]
  (if (= (count tiles) 0)
    #{(quantize-position [0 0 0])}
    (->> (keys tiles)
         (map #(get-empty-connected-neighbours tiles %))
         (apply concat)
         (filter #(< (vec3-length %) max-radius))
         (set)
      )))

(defn get-empty-positions-2 [ts]
  (if (= (count (ts :tiles)) 0)
    #{(quantize-position [0 0 0])}
    (ts :empty)))


(defn test-empty-status [ts]
  (let [tiles (ts :tiles)
        empty-positions (get-empty-positions tiles (get-in ts [:params :max-radius]))
        computed-empty (map #(vec [% (get-empty-status tiles %)]) empty-positions)
        error (into {} (map #(vec [% (contains? (ts :empty) %)]) empty-positions))
        ]
;      (filter #(not (get-empty-status tiles %))  empty-positions)
    error
    )
  )


(defn find-candidates [neighbourhood tileset dead]
  (let [outercode (get-outer-facecode2 neighbourhood)]
    (if (contains? dead outercode)
      ()
      (filter #(tilecodes-directly-compatible? outercode %) tileset))
   )
)


; this version does not force connectivity
(defn get-empty-positions-nfc [tiles max-radius]
  (if (= (count tiles) 0)
    #{[0 0 0]}
    (->> (keys tiles)
         (map get-neighbours)
         (apply concat)
         (filter #(is-empty? tiles %))
         (filter #(< (vec3-length %) max-radius))
         (set))))


(defn add-to-dead-loci-ts [ts code]
  (assoc ts :dead (conj (ts :dead) code)))

;(defn add-to-dead-loci-ts2 [ts code]
;  (assoc ts :dead (clojure.set/union (ts :dead) #{code})))

(defn add-to-dead-loci-ts2 [ts code]
  (assoc ts :dead (clojure.set/union (ts :dead) code)))

(defn get-untileable-neighbours [tiles tileset pos dead]
  (->> (get-neighbours pos)
       (filter #(= (count (find-candidates (get-neighbourhood tiles %) tileset dead)) 0))))


;(defn creates-untileable-region? [_tiles tileset pos]
;  (> (count (->> (get-connected-neighbours _tiles pos)
;                 (filter #(and (is-empty? _tiles %)
;                               (or (contains? @dead-loci
;                                              (get-outer-facecode2 (get-neighbourhood _tiles %)))
;                                   (= (count (find-candidates2 (get-neighbourhood _tiles %) tileset)) 0))))))
;     0))


(defn creates-untileable-region-ts? [ts pos]
  (let [tiles (ts :tiles)
        tileset (ts :tileset-expanded)
        dead (ts :dead)]
    (> (count (->> (get-connected-neighbours tiles pos)
                   (filter #(and (is-empty? tiles %)
                               (or (contains? dead (get-outer-facecode2 (get-neighbourhood tiles %)))
                                   (= (count (find-candidates (get-neighbourhood tiles %) tileset dead)) 0))))))
     0)))


; this version does not force connectivity
;(defn creates-untileable-region? [_tiles tileset pos]
;  (> (count (->> (get-neighbours pos)
;                 (filter #(and (is-empty? _tiles %)
;                               (or (contains? @dead-loci
;                                              (get-outer-facecode2 (get-neighbourhood _tiles %)))
;                                   (= (count (find-candidates2 (get-neighbourhood _tiles %) tileset)) 0))))))
;     0))


(defn find-best-positions-ts [ts empty-positions]
  (filter #(< (count (find-candidates (get-neighbourhood (ts :tiles) %)
                                      (ts :tileset-expanded)
                                      (ts :dead))) 2)
          empty-positions))


(defn find-any-positions-ts [ts empty-positions]
  (filter #(> (count (find-candidates (get-neighbourhood (ts :tiles) %)
                                      (ts :tileset-expanded)
                                      (ts :dead))) 0)
          empty-positions))


(defn choose-positions-ts [ts empty-positions]
  (let [tiles (ts :tiles)
        tileset (ts :tileset-expanded)
        best (find-best-positions-ts ts empty-positions)]
    (if (= (count best) 0)
      (find-any-positions-ts ts empty-positions)
      best)))


; check if placing tile code at pos creates any dead regions in its
; neighbourhood
; false = has dead neighbours
; true = ok to place tile at pos
(defn test-tile-at-pos [ts pos code]
  (let [new-ts (make-tile-2 ts pos code)
        tiles (new-ts :tiles)
        tileset (new-ts :tileset-expanded)
        dead (new-ts :dead)
        nbs (get-empty-neighbours tiles pos) ]
    (= (count (->> nbs
                   (filter #(or ;(contains? dead %)
                                (contains? dead (get-outer-facecode2 (get-neighbourhood tiles %)))
                                ;(= (count (find-candidates (get-neighbourhood tiles %) tileset dead)) 0)
                                ))
                   ))
     0))
  )





;(defn choose-tilecode [neighbourhood tileset dead]
;  ;(println "choose-tilecode " neighbourhood tileset dead)
;  (let [candidates (find-candidates neighbourhood tileset dead)]
;    (if (seq candidates)
;      (rand-nth candidates)
;      nil)))

(defn choose-tilecode [ts pos]
  (let [neighbourhood (get-neighbourhood (ts :tiles) pos)
        candidates (find-candidates neighbourhood (ts :tileset-expanded) (ts :dead))
        candidates-filtered candidates]
        ;candidates-filtered (filter #(test-tile-at-pos ts pos %) candidates)]
    (if (seq candidates)
      (rand-nth candidates)
      nil)))



; Receive a vector of positions and return the closest to the center
; ie the vector with the shortest length. If there are more than one
; with length equal to the shortest length then return a random one.
(defn find-closest-to-center [positions]
  (if (> (count positions) 0)
    (let [sorted (->> (map #(vec [%1 (vec3-sum-of-squares %1)]) positions)
                      (sort-by #(% 1)))
          min-length ((first sorted) 1)
          tie-winners (filter #(= min-length (% 1)) sorted)]
      (if (= 1 (count tie-winners))
        ((first tie-winners) 0)
        ((rand-nth tie-winners) 0)))
    [0 0 0]))


(defn find-closest-to-point [positions point]
  (let [lengths (into {} (map #(vec [%1 (vec3-sum-of-squares (vec3-sub %1 point))])
                              positions))
        sorted-lengths (sort-by #(vec3-sum-of-squares (key %)) lengths)
        min-length ((first sorted-lengths) 1)
        tie-winners (filter #(= min-length (val %)) sorted-lengths) ]
    (if (= 1 (count tie-winners))
      ((first tie-winners) 0)
      ((rand-nth tie-winners) 0))))


(defn get-tile-freqs [tiles]
  (->> tiles
       vals
       (map normalize-tilecode)
       frequencies
       (sort-by val)
       reverse))



(defn uuid [] (str (java.util.UUID/randomUUID)))

; some useful trunc-octa tiles:
; "-3---3-----3-3" ; tetrahedron
; "-3--333-3--333" ; octahedron
; "3-33---3-33---" ; cube
; "-33----3--3---" ; cubic corner piece

(def default-params {
  ;:tileset ["----1A---a----"] ; to
  ;:tileset ["----1A---a--"] ; rd
  :tileset ["111111"] ; cube
  ;:tileset ["---1" "-1-1" "--11"] ; sq
  ;:tileset ["1-1---" "1--1-1"] ; hex
  :seed ""
  :max-iters 1000000
  :max-radius 8
  :max-tiles 1000000
  :adhd 2.0
  :autism 1.0
  :nfc? false 
  })



(def default-state {
  :uuid ""
  :params default-params
  :tiles (ordered-map)
  :tileset-expanded #{}
  :dead #{}
  :empty #{}
  :iters 0
  :solved false
  :run-status :runnable  ; :runnable :halted
  })


(defn make-params
  [& {:keys [tileset seed max-iters max-radius max-tiles adhd autism best-of nfc?]
      :or {tileset (default-params :tileset)
           seed (default-params :seed)
           max-iters (default-params :max-iters)
           max-radius (default-params :max-radius)
           max-tiles (default-params :max-tiles)
           adhd (default-params :adhd)
           autism (default-params :autism)
           nfc? (default-params :nfc?)
           } } ]
  {
  :tileset tileset
  :seed (if (< (count seed) (@current-topology :num-faces)) (rand-nth (vec tileset)) seed)
  :max-iters max-iters
  :max-radius max-radius
  :max-tiles max-tiles
  :adhd adhd
  :autism autism
  :nfc? nfc?
  :tileset-number (tileset-to-number tileset)
  } )


(defn make-state
  ([] (make-state (make-params)))
  ([params]
    (-> default-state
        (assoc :tileset-expanded (expand-tiles-preserving-symmetry (params :tileset)))
        (assoc :params params)
        (assoc :tiles (ordered-map (quantize-position [0 0 0]) (params :seed)))
        (assoc :empty (if (params :nfc?)
                        (set (get-empty-neighbours {[0 0 0] (params :seed)} [0 0 0]))
                        (set (get-empty-connected-neighbours {[0 0 0] (params :seed)} [0 0 0]))))
        ;(assoc :empty (set (get-empty-connected-neighbours {[0 0 0] (params :seed)} [0 0 0])))
        (assoc :uuid (uuid))
      )))


; move this definition to core
(def tiler-state (atom nil)) ; (atom (make-state)))
(def tiler-thread (atom nil))


(defn compute-backtrack-amount [num-tiles autism adhd]
  (loop [n 1]
    (if (or (>= n num-tiles)
            (> (rand) (Math/pow (/ n (+ n autism)) adhd)))
      n
      (recur (inc n)))))


(defn backtrack-non-zero [tiles autism adhd]
  (let [num-tiles (count tiles)
        n (compute-backtrack-amount num-tiles autism adhd)
        ni (- num-tiles n)]
    (if (and (> num-tiles 1)
             (< n num-tiles))
      (do
        ;(append-stats-buffer! stats-backtrack n)
        ;(when sonify?
        ;  (osc-send client2 "/rhombrick.tiling" "backtrack"
        ;            (int (mod (tilecode-to-number (val (last (take ni tiles)))) 21))))
        (ordered-map (take ni tiles)))
      (do
        ;(append-stats-buffer! stats-backtrack 0)
        tiles))))



; empty cell bookkeeping for backtracking
(defn update-empty [ts remaining discarded]
  (if (zero? (count remaining))
    #{(quantize-position [0 0 0])}
    (let [max-rad (get-in ts [:params :max-radius])
          nb-positions (->> (map get-neighbours discarded)
                            (apply concat))
          all-positions (apply conj nb-positions discarded)
          empty-fn (if (get-in ts [:params :nfc?]) get-empty-status-nfc get-empty-status)
          empty-status (into {} (map #(vec [% (empty-fn remaining %)])
                                     all-positions))
          to-add (->> empty-status
                      (filter (fn [s] (true? (val s))))
                      (keys)
                      (filter #(< (vec3-length %) max-rad))
                      (set))
          to-del (->> empty-status
                      (filter (fn [s] (false? (val s))))
                      (keys)
                      (set))]
      (-> (ts :empty)
          (clojure.set/union to-add)
          (clojure.set/difference to-del)))))


; backtrack with empty cell bookkeeping
(defn backtrack-non-zero-bk [ts]
  (let [num-tiles (count (ts :tiles))
        bt-num (compute-backtrack-amount
                num-tiles
                (get-in ts [:params :autism])
                (get-in ts [:params :adhd]))
        split-point (- num-tiles bt-num )
        [remaining-tiles discarded] (split-at split-point (ts :tiles))]
    ;(when sonify?
    ;      (osc-send client2
    ;                "/rhombrick.tiling" "backtrack"
    ;                (int (mod (tilecode-to-number (val (last (take split-point (ts :tiles)) ))) 21))))
    (if (and (> num-tiles 1)
             (> split-point 0)
             (< split-point num-tiles))
      (-> ts
          (assoc :tiles (ordered-map remaining-tiles))
          (assoc :empty (update-empty ts (ordered-map remaining-tiles) (keys discarded)))
          )
      ts)))




(defn backtrack-non-zero-ts [ts]
  (assoc ts :tiles (backtrack-non-zero (get ts :tiles)
                                       (get-in ts [:params :autism])
                                       (get-in ts [:params :adhd]))))


;(defn backtrack [_tiles]
;  (let [num-tiles (count _tiles)
;        n (compute-backtrack-amount num-tiles @autism @adhd)
;        ni (- num-tiles n)]
;    (if (and (> num-tiles 0)
;             (<= n num-tiles))
;      (do
;        ;(append-stats-buffer! stats-backtrack n)
;        (take ni _tiles))
;      (do
;        ;(append-stats-buffer! stats-backtrack 0)
;        _tiles))))


(defn backtrack-n [tiles n]
  (let [num-tiles (count tiles)
        ni (- num-tiles n)]
    (if (and (> num-tiles 0)
             (<= n num-tiles))
      (do
        ;(append-stats-buffer! stats-backtrack n)
        (ordered-map (take ni tiles)))
      (do
        ;(append-stats-buffer! stats-backtrack 0)
        tiles))))


(defn inc-iters [ts]
  (update-in ts [:iters] inc))


(defn cache-dead-nbhood [ts pos]
  (let [tiles (ts :tiles)
        tileset (ts :tileset-expanded)
        dead (ts :dead)
        untileable (get-untileable-neighbours tiles tileset pos dead)
        outercodes (into #{} (map #(get-outer-facecode2 (get-neighbourhood tiles %)) untileable)) ]
    (if (> (count outercodes) 0)
      (let [dead (apply conj (ts :dead) outercodes) ]
        (assoc ts :dead dead))
      ts)))



(defn make-backtracking-tiling-iteration4 [ts]
  (let [{:keys [params tiles dead iters solved]} ts
        ;tileset (ts :tileset-expanded)
        ;empty-positions (get-empty-positions tiles (params :max-radius))]
        ;empty-positions (ts :empty)
        empty-positions (get-empty-positions-2 ts)

        tileset (ts :tileset-expanded)
        ]
    ;(println ts)
    (if (zero? (count empty-positions))
      (-> ts
          (assoc :run-status :halted)
          (assoc :solved true))

      (if-let [positions (choose-positions-ts ts empty-positions)]
        (let [;new-pos (find-closest-to-center positions)
              new-pos (find-closest-to-point positions @assemblage-center)
              new-neighbourhood (get-neighbourhood tiles new-pos)
              new-code (choose-tilecode ts new-pos)]
              ;new-code (choose-tilecode new-neighbourhood tileset)]
          ;(println "pos nb code tileset" new-pos new-neighbourhood new-code tileset)
          (if (nil? new-code)
            ; no tile will fit, backtrack and return new state
            (-> ts
                ;(add-to-dead-loci-ts (get-outer-facecode2 new-neighbourhood))
                (add-to-dead-loci-ts2 (set (get-code-symmetries (get-outer-facecode2 new-neighbourhood))))
                (backtrack-non-zero-bk)
                (inc-iters))
            ; else add tile and return new state
            (-> (make-tile-2 ts new-pos new-code)
                (inc-iters))))

        (-> ts
          (assoc :run-status :halted))))))


(defn make-backtracking-tiling-iteration5 [ts]
  (let [{:keys [params tiles dead iters solved]} ts
        empty-positions (get-empty-positions-2 ts)
        tileset (ts :tileset-expanded)]
    (if (zero? (count empty-positions))
      (-> ts
          (assoc :run-status :halted)
          (assoc :solved true))
      (if-let [positions (choose-positions-ts ts empty-positions)]
        (let [;new-pos (rand-nth (vec empty-positions))
              new-pos (find-closest-to-point positions @assemblage-center)
              ;new-pos (find-closest-to-center positions)
              new-neighbourhood (get-neighbourhood tiles new-pos)
              candidates (find-candidates new-neighbourhood tileset (ts :dead))
              candidates-strict (filter #(test-tile-at-pos ts new-pos %) candidates)
              new-code (if (seq candidates-strict)
                         (rand-nth candidates-strict)
                         (if (seq candidates)
                           (rand-nth candidates) nil))]
          (if (zero? (count candidates))
            ; no tile will fit, backtrack and return new state
            (-> ts
                (add-to-dead-loci-ts2 (get-outer-facecode2 new-neighbourhood))
                ;(add-to-dead-loci-ts2 (set (get-code-symmetries (get-outer-facecode2 new-neighbourhood))))
                (backtrack-non-zero-bk)
                (inc-iters))
            ; else add tile and return new state
            (if (test-tile-at-pos ts new-pos new-code)
              (-> ts
                  (make-tile-2 new-pos new-code)
                  (inc-iters))
              (-> ts
                  (backtrack-non-zero-bk)
                  (inc-iters)
              ))))
        (-> ts
          (assoc :run-status :halted))))))


(defn halt-tiler []
  (reset! tiler-state (-> @tiler-state (assoc :run-status :halted)))
  (println "tiler-state -> halted"))


(defn tiler-can-iterate? [ts]
  (and (= (ts :run-status) :runnable)
       (< (count (ts :tiles)) (get-in ts [:params :max-tiles]))
       (< (ts :iters) (get-in ts [:params :max-iters]))
       ;(not (and (= (ts :iters) 1000) (< (count (ts :tiles)) 50))) ; early bailout
       ))


(defn run-backtracking-tiling-thread [ts]
  (println "tiler thread starting with state:" ts)
  (reset! tiler-state ts)
  (while (tiler-can-iterate? @tiler-state)
    ;(println "iter")
    (let [iter-start-time (System/nanoTime)]
;      (dosync
        (swap! tiler-state make-backtracking-tiling-iteration5)
        (update-assemblage-center (@tiler-state :tiles))
        (reset! last-iteration-time (float (/ (- (System/nanoTime) iter-start-time) 1000000.0)))
;        )
      ))
  (halt-tiler))


(defn cancel-tiler-thread []
  (when (future? @tiler-thread)
    (future-cancel @tiler-thread)
    (if (or (future-cancelled? @tiler-thread)
            (future-done? @tiler-thread))
      (halt-tiler)
      (println "cancel-tiler-thread failed"))))


(defn start-tiler [tileset soft-start?]
  (cancel-tiler-thread)
  (Thread/sleep 100)
  (let [ts (make-state (make-params :tileset tileset
                                    :seed (rand-nth (vec tileset))
                                    ;:max-radius @assemblage-max-radius
                                    ;:adhd @adhd
                                    ;:autism @autism
                                    :nfc? false
                                    )) ]
    (if soft-start?
      (reset! tiler-state (assoc ts :dead (@tiler-state :dead)))
      (reset! tiler-state ts))
    (reset! tiler-thread (future (run-backtracking-tiling-thread @tiler-state)))))




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

; History Tracking
; ================
;
; To avoid attempting to tile previously failed paths through tiling space,
; we need to store a history of tiles placed. Let history-paths be a vector
; containing sequences of tiles placed between backtracking (or start) events.
; Each entry of history-paths represents a failed path through the search
; space. There are two exceptions to this: 1) If the tiler completes, then
; the last entry of history-paths contains a successful path. 2) While the
; tiler is running, the current history-path is not known to be a failure path.
;
;
; Populating and maintaining the history vector
; ---------------------------------------------
;
; When the tiler starts, or performs backtracking,  a new entry is created in
; the history vector. Each entry in the history vector is a vector containing the
; tiles layed on this path. A new path is created after backtracking, and
; is complete when the next backtrack occurs. Subsequent tile placements will
; be added to the newly created path.
;
; We will need an atom current-history-path-idx to contain the index of the
; current history path.
;
; Using the history vector
; ------------------------
;
; When examining a candidate tile for a position, after passing compatibility,
; the tile will be conjed to the current-history-path to create
; candidate-history-path. If the history-paths contains a sequence matching[1]
; candidate-history-path then the tile should not be accepted for the
; position.
;
; [1] There are details not yet worked out concerning what a "matching path"
;     exactly means. Do we need the paths to match from beginning to end? Do
;     we consider it a match if the current history path is a subvector of a
;     known failure path, or perhaps only if it matches at the end of the
;     containing path?
;
; It seems that we should consider the following scenarios a match:
;
;  1) A path in the history vector is identical to candidate-history-path.
;  2) A path in the history vector *ends with* candidate-history-path.
;  3) If a path we are testing is shorter than candidate-history-path, then it
;     can be considered a match if the candidate-history-path ends with the
;     test path.
;  4) ...?
;
; In terms of implementation, when searching we can ignore all history paths
; which don't end with the same tile as the last tile in candidate-history-path.
;
; It would be nice to use a hashmap for the history paths, with the world
; coord of the path's final tile as the key. There will be potentially many
; paths like this, so we could then make the map value a vector of paths with a
; given final tile.
;
; It is still not clear to me whether this history tracking idea would provide
; a great improvement over the current simpler tactic of caching dead loci and
; checking that no known dead loci are created by placing a tile.
;
; Possible data structures for history-paths:
;
; vector version:
; (def history-paths [
;   [[[0 0 0] "-0---d-----D"] [[-1 0 1] "-6---d------"] [[1 0 -1] "-0---d-----D"]]
;   [[[-1 0 1] "-6---d------"] [[1 0 -1] "-0---d-----D"] [[1 0 1] "-D-----0----"]]
;   etc...
; ])
;
; map version:
; (def history-paths {
;   [[1 0 -1] "-0---d-----D"] [[[0 0 0] "-0---d-----D"] [[-1 0 1] "-6---d------"] [[1 0 -1] "-0---d-----D"]]
;   [[1 0 1] "-D-----0----"] [[[-1 0 1] "-6---d------"] [[1 0 -1] "-0---d-----D"] [[1 0 1] "-D-----0----"]]
;   etc...
; })
;


; _______________________________________________________________________


; Idea: when selecting a tile for a position, if there is more than one
; candidate then sum the number of candidates for each neighbour as if the tile
; was chosen. The tile with the greatest number of neighbour candidates
; (divided by the number of connection sites?) is chosen.
; The reason for trying this is that I think it may cause the tiler to be less
; likely to generate repetitive tilings and may alse help somewhat to avoid
; dead ends.
;
;(defn compute-compatibility-score [_tiles candidate pos tileset]
;  (let [test-tiles (assoc _tiles pos candidate)
;        neighbours-pos (get-empty-connected-neighbours test-tiles pos)
;        num-connectable (count neighbours-pos)
;        nb-candidates (map #(find-candidates2 (get-neighbourhood test-tiles %) tileset)
;                           neighbours-pos)
;        num-nb-candidates (reduce + (map count nb-candidates))]
;    (if (and (> num-connectable 0)
;             (> num-nb-candidates 0))
;      (/ (double num-nb-candidates) num-connectable)
;      0.0)))
;
;
;(defn find-most-compatible [_tiles pos tileset candidates]
;  (->> (map #(vec [% (compute-compatibility-score _tiles % pos tileset)])
;                    candidates)
;       (into {})
;       (sort-by val >)
;       #(key (first %))))
;
