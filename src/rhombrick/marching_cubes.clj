(ns rhombrick.marching-cubes
  (:use [rhombrick.marching-cubes-tables]
        [rhombrick.vector]
        [rhombrick.staticgeometry]
        [rhombrick.bezierbox]))





(def ^:const grid-vertices [[0 0 0]
                         [1 0 0]
                         [1 1 0]
                         [0 1 0]
                         [0 0 1]
                         [1 0 1]
                         [1 1 1]
                         [0 1 1]])

(def topo-coord-scales {:square 1.0
                        :cube 1.0 
                       :hexagon 1.0
                       :rhombic-dodecahedron 0.5
                       :truncated-octahedron 0.5})

(defn lerp- [t a b]
  (+ a (* t (- b a))))

(defn sphere-func [xyz r]
  (if (< (vec3-length xyz) r)
    [1 (xyz 0) (xyz 1) (xyz 2)]
    [0 (xyz 0) (xyz 1) (xyz 2)]))



(defn spheres-func [xyz r]
  (if (or 
          (< (vec3-distance [(- 0.5) 0 0] xyz) r)
          (< (vec3-distance [0.5 0 0] xyz) r)
          (< (vec3-distance [0 (- 0.5) 0] xyz) r)
          (< (vec3-distance [0 0.5 0] xyz) r)
          (< (vec3-distance [0 0 (- 0.5)] xyz) r)
          (< (vec3-distance [0 0 0.5] xyz) r)
        
        )
    [1 (xyz 0) (xyz 1) (xyz 2)]
    [0 (xyz 0) (xyz 1) (xyz 2)]))


;(defn fuzzy-sphere-func [sample-xyz r fr]
;  (if (< (vec3-length sample-xyz) (+ r fr))
;    (let [dist (- r )] [1 0 0 0]
;    [0 0 0 0])))

(defn sin-combo [xyz a]
  (let [s1 (+ 0.5 (* 0.5 (Math/sin (* (xyz 0) 13.00135))))
        s2 (+ 0.5 (* 0.5 (Math/sin (* (xyz 1) 21.00181))))
        s3 (+ 0.5 (* 0.5 (Math/sin (* (xyz 2) 8.00132)))) 
        v  (+ s1 s2 s3)]
    [v s1 s2 s3]
  ))

(defn is-below [p face-center]
  (if (< (vec3-dot (vec3-sub p face-center) face-center) 0) 1.0 0.0))


; input a point and a vector of vertices representing the face centers of a 
; convex polyhedron. return a vector of length 4. The first number is 0.0 or
; 1.0 depending on whether the point is contained within the polyhedron. The
; next three numbers are the components of the face normal closest to
;  the given point. 
;(defn polyhedron-contains? [xyz face-centers]
(def polyhedron-contains? (memoize (fn [xyz face-centers]
;  (let [closest-f (first (sort-by #(vec3-distance xyz %) face-centers)) ]
    (if (= (count face-centers)
           (count (filter #(> % 0) (map #(is-below xyz (vec3-scale % (topo-coord-scales (@current-topology :id))))
                                        face-centers))))
           ;(count (filter #(> % 0) (map #(is-below xyz (vec3-scale % 0.5))
           ;                             face-centers))))
      true
      false))))
;)
      ;(vec (flatten [0.0 (vec3-normalize closest-f)]))
      ;(vec (flatten [1.0 (vec3-normalize closest-f)])))))

(defn tilecode-planes-contain [xyz code topo]
  (let [face-idxs (get-connected-idxs code)
        face-centers (map #((topo :face-centers) %) face-idxs)]
    (if (= (count face-centers)
           (count (filter #(> % 0) (map #(is-below xyz (vec3-scale % (topo-coord-scales (@current-topology :id))))
                                        face-centers))))
      [1.0 1.0 1.0 1.0]
      [0.0 1.0 1.0 1.0])))



;(defn get-points-for-curve [f1-idx f2-idx npoints topo]
(def get-points-for-curve (memoize (fn [f1-idx f2-idx npoints topo]
  (let [step (double (/ 1.0 npoints))]
    (->> (map #(get-bezier-point-3d f1-idx f2-idx (* % step)) (range (inc npoints)))
         (map #(vec3-scale % (topo-coord-scales (topo :id))))
         vec)))))


; find the closest ps to p. returns [closest distance normal t]
(defn find-closest-point [p ps]
  (let [npoints (count ps)]
    (loop [p p
           ps ps
           best nil
           i 0]
      (if (zero? (count ps))
        [(best 0) (Math/sqrt (best 1)) (best 2) (best 3)]
        (let [test-p (first ps)
              dist-sqr (vec3-distance-squared p test-p)
              t (double (/ i npoints))]
          (cond
            (nil? best)
              (recur p (vec (rest ps)) (vec [test-p dist-sqr [1 0 0] t]) (inc i))
            (< dist-sqr (best 1))
              (recur p (vec (rest ps)) (vec [test-p dist-sqr (vec3-normalize (vec3-sub p test-p)) t]) (inc i))
            :else
              (recur p (vec (rest ps)) (vec best) (inc i))))))))



(defn cell-func [xyz topo]
  (let [closest-f (first (sort-by #(vec3-distance xyz %) (topo :face-centers))) ]
;    (if (= (count (topo :face-centers))
;           (count (filter #(> % 0) (map #(is-below xyz (vec3-scale % (topo-coord-scales (topo :id))))
;                                        (topo :face-centers)))))
      (let [n (vec3-normalize closest-f)
            d (distance-point-to-plane xyz closest-f n)
            field (/ 1.0 (* d d))]
        [field (n 0) (n 1) (n 2)])))


; bounding sphere radii : 
;   trunc-oct: 2.449489742783178
;   rhomb-dodeca: 2.0
;   cube: 1.7320508075688772

(defn tilecode-distance-avg-blob [xyz f1-idx f2-idx r1 r2]
  (let [;curve-points (map #(vec3-scale % (topo-coord-scales (@current-topology :id)))
        ;                  (get-points-for-curve f1-idx f2-idx 16 @current-topology))
        curve-points (get-points-for-curve f1-idx f2-idx 16 @current-topology)

        rad 0.3
        dists (map #(/ 1.0 (- (* rad rad) (vec3-distance-squared xyz %))) curve-points)
        ;dist-avg (/ (reduce + dists) (count curve-points))
        dist-sum (reduce + dists)
        ;[closest-p closest-d n] (find-closest-point xyz curve-points)
        ;t-at-closest-p (get-t-for-bezier-point closest-p curve-points)
        ;radius-at-p (lerp- t-at-closest-p r1 r2)
        ]
    [dist-sum 0.0 0.0 0.0]))



(defn tilecode-bezier-blob [xyz code topo]
;(def tilecode-bezier-blob (memoize (fn [xyz code topo]
  (if (> (vec3-length xyz) 1.224744871391589)
          ;(not (polyhedron-contains? (vec3-scale xyz 0.9375) (topo :face-centers))))
    [999.0 0.0 0.0 0.0]
    (let [curve-res 16 
          endpoint-pairs (vec (-make-curve-endpoints (get-connected-idxs code)))
          curves-points (map-indexed #(vec [%1 (get-points-for-curve (%2 0) (%2 1) curve-res topo) ])
                                     endpoint-pairs)
          closest-per-curve (map-indexed #(vec [%1 (find-closest-point xyz (%2 1))]) curves-points)
          closest-data (first (sort-by #((% 1) 1) closest-per-curve))
          closest-idx (first closest-data)
          [closest-p closest-d closest-n closest-t] (second closest-data)
          r1 (bezier-box-thicknesses (.charAt code (first (endpoint-pairs closest-idx)))) 
          r2 (bezier-box-thicknesses (.charAt code (second (endpoint-pairs closest-idx))))
          radius-at-p (lerp- closest-t (/ r1 4.0) (/ r2 4.0))
          n closest-n]
      [(* (- closest-d radius-at-p) 2.0) (n 0) (n 1) (n 2)]))

;      (if (< closest-d radius-at-p)
;        [0.0 (n 0) (n 1) (n 2)]
;        [1.0 (n 0) (n 1) (n 2)])))
  )
;;))


(defn tilecode-bezier-blob2 [xyz code topo]
  (if ;(> (vec3-length xyz) 1.224744871391589)
      (not (polyhedron-contains? (vec3-scale xyz 1.0) (topo :face-centers)))
    [999.0 0.0 0.0 0.0]
    (let [curve-res 16 
          endpoint-pairs (vec (-make-curve-endpoints (get-connected-idxs code)))
          curves-points (map-indexed #(vec [%1 (get-points-for-curve (%2 0) (%2 1) curve-res topo) ])
                                     endpoint-pairs)
          closest-per-curve (map-indexed #(vec [%1 (find-closest-point xyz (%2 1))]) curves-points)
          closest-data (first (sort-by #((% 1) 1) closest-per-curve))
          closest-idx (first closest-data)
          ;field-strengths (map #(/ 1.0 (* (% 1) (% 1))) closest-per-curve) 

          [closest-p closest-d closest-n closest-t] (second closest-data)
          r1 (bezier-box-thicknesses (.charAt code (first (endpoint-pairs closest-idx)))) 
          r2 (bezier-box-thicknesses (.charAt code (second (endpoint-pairs closest-idx))))
          radius-at-p (lerp- closest-t (/ r1 1.0) (/ r2 1.0))
          n closest-n
          d (- closest-d radius-at-p)
          field (/ 1.0 (* d d))
          ]
      ;[(* (- closest-d radius-at-p) 2.0) (n 0) (n 1) (n 2)]))
      [field (n 0) (n 1) (n 2)]))

;      (if (< closest-d radius-at-p)
;        [0.0 (n 0) (n 1) (n 2)]
;        [1.0 (n 0) (n 1) (n 2)])))
  )
;))


(defn make-surface-cache-obj [code dim mesh]
  {:code code
   :dim dim
   :mesh mesh})


(def surface-thread (atom nil))



(def vert-to-grid-indices
  [[0 1]
   [1 2]
   [2 3]
   [3 0]
   [4 5]
   [5 6]
   [6 7]
   [7 4]
   [0 4]
   [1 5]
   [2 6]
   [3 7]])


(defn- normalize [x y z]
  (let [mag (Math/sqrt (+ (* x x) (* y y) (* z z)))]
    [(/ x mag) (/ y mag) (/ z mag)]))


(defn cube-index [grid isolevel]
  (let [value 0
        value (if (< (grid 0) isolevel) (bit-or value 1) value)
        value (if (< (grid 1) isolevel) (bit-or value 2) value)
        value (if (< (grid 2) isolevel) (bit-or value 4) value)
        value (if (< (grid 3) isolevel) (bit-or value 8) value)
        value (if (< (grid 4) isolevel) (bit-or value 16) value)
        value (if (< (grid 5) isolevel) (bit-or value 32) value)
        value (if (< (grid 6) isolevel) (bit-or value 64) value)
        value (if (< (grid 7) isolevel) (bit-or value 128) value)]
    value))


(defn vertex-interp [isolevel p1 p2 v1 v2]
  (cond
    (< (Math/abs (double (- isolevel v1))) 0.00001) p1
    (< (Math/abs (double (- isolevel v2))) 0.00001) p2

    :else
    (let [mu (/ (- isolevel v1) (- v2 v1))
          x (lerp- mu (p1 0) (p2 0))
          y (lerp- mu (p1 1) (p2 1))
          z (lerp- mu (p1 2) (p2 2))]
      [x y z])))


(defn- vertex-position [vert-index grid isolevel]
  (let [grid-idxs (vert-to-grid-indices vert-index)
        p1 (grid-vertices (grid-idxs 0))
        v1 (grid (grid-idxs 0))
        p2 (grid-vertices (grid-idxs 1))
        v2 (grid (grid-idxs 1))]
    (vertex-interp isolevel p1 p2 v1 v2)))


(defn polygonise [grid isolevel]
  (let [index (cube-index grid isolevel)]
    (map #(vertex-position % grid isolevel) (tri-table index))))


(defn make-tilecode-bezier-blob-surface [isolevel code xdim ydim zdim]
  (let [xstep (/ 2.0 xdim)
        ystep (/ 2.0 ydim)
        zstep (/ 2.0 zdim)
        scale-vert (fn [v o]
                     (let [v (map * v [xstep ystep zstep]) ; scale
                           v (map + v o)]                  ; offset
                       (into [] v)))

        ;; generate the integer grid indices
        grid-indices (for [xidx (range xdim)
                           yidx (range ydim)
                           zidx (range zdim)]
                       [xidx yidx zidx])

        ;; build the surface over a grid with that many indices but
        ;; covering dims -1, 1
        surface (map
                 (fn [[xidx yidx zidx]]
                   (let [offset [(- (* xidx xstep) 1)
                                 (- (* yidx ystep) 1)
                                 (- (* zidx zstep) 1)]
                         grid (into [] (map (fn [v]
                                              (let [v (scale-vert v offset)
                                                   ;[n _ _ _] (sin-combo v 0.4)]
                                                   ;[n _ _ _] (if (polyhedron-contains? v (@current-topology :face-centers))
                                                   ;            [1.0 1.0 1.0 1.0]
                                                  ;             [0.0 1.0 1.0 1.0])]
                                                   ;[n _ _ _] (bezier-blob v 0 1 0.12 0.4)]
                                                   [n _ _ _] (cell-func v @current-topology)]
                                                   ;[n _ _ _] (tilecode-planes-contain v code @current-topology)]
                                                   ; [n _ _ _] (tilecode-bezier-blob2 v code @current-topology)]
                                                    ;[n _ _ _] (tilecode-distance-avg-blob v 0 2 0.2 0.5)]
                                                   ;[n _ _ _] (spheres-func v 0.4)]
                                                n))
                                            grid-vertices))
                         
                         base-tris (polygonise grid isolevel)
                         tris (map #(scale-vert % offset) base-tris)
                         norms (map (fn [v]
                                      ;(let [[_ nx ny nz] (sin-combo v 0.4)]
                                      ;(let [[_ nx ny nz] (if (polyhedron-contains? v (@current-topology :face-centers))
                                      ;                     [1.0 1.0 1.0 1.0]
                                      ;                         [0.0 1.0 1.0 1.0])]
                                      ;(let [[_ nx ny nz] (bezier-blob v 0 1 0.12 0.4)]
                                      (let [[_ nx ny nz] (cell-func v @current-topology)]
                                      ;(let [[_ nx ny nz] (tilecode-bezier-blob2 v code @current-topology)]
                                      ;(let [[_ nx ny nz] (tilecode-distance-avg-blob v 0 2 0.2 0.5)]
                                      ;(let [[_ nx ny nz] (tilecode-planes-contain v code @current-topology)]

                                      ;(let [[_ nx ny nz] (spheres-func v 0.4)]
                                        (normalize nx ny nz)))
                                    tris)]
                     
                     {:tris tris :norms norms}))
                 
                 grid-indices)]

    {:tris (mapcat :tris surface)
     :norms (mapcat :norms surface)}))


(def test-surface (atom {}))

(def tileset-meshes (atom {}))


(defn remove-endcap-triangles [mesh topo]
  (let [idxs-to-remove (->> (mesh :tris)
                            (partition 3)
                            (map-indexed #(vec [%1 %2]))
                            (filter #(not (polyhedron-contains? (second %) (topo :face-centers)))))]
                            ;(map first))]
    idxs-to-remove
    ))


(defn make-tileset-meshes-old [isolevel tileset xdim ydim zdim ]
  (println "make-tileset-meshes" isolevel tileset xdim ydim zdim)
  (reset! tileset-meshes {})
  (doseq [code tileset]
    (print code "...")
    (swap! tileset-meshes assoc code (make-tilecode-bezier-blob-surface isolevel code xdim ydim zdim))
    (println "done"))
  )


(defn prioritise-tiles [ts]
  (let [freqs (frequencies (vals (ts :tiles)))
        sorted (vec (reverse (sort-by #(get freqs % 0) (ts :tileset-expanded))))]
    sorted
  ))

(defn make-tileset-meshes [ts isolevel xdim ydim zdim ]
  (println "make-tileset-meshes" isolevel (ts :tileset-expanded) xdim ydim zdim)
  (reset! tileset-meshes {})
  (doseq [code (prioritise-tiles ts)]
    (when-not (contains? @tileset-meshes code)
      (print code "...") 
      (swap! tileset-meshes assoc code (make-tilecode-bezier-blob-surface isolevel code xdim ydim zdim)))
    (println "done"))
  )

; use geometrical transform to generate tileset symmetries
(defn make-tileset-meshes-with-rotations [ts isolevel xdim ydim zdim ]
  (println "make-tileset-meshes" isolevel (ts :tileset-expanded) xdim ydim zdim)
  ;(reset! tileset-meshes {})
  (doseq [code (get-in ts [:params :tileset])]
    (let [identity-mesh (make-tilecode-bezier-blob-surface isolevel code xdim ydim zdim)]
      (doseq [i (range (count symmetries-flattened))]
        (let [ang (first (symmetries-flattened i))
              axis (second (symmetries-flattened i))
              new-tris (map #(rotate-point % axis ang) (identity-mesh :tris))
              new-norms (map #(rotate-point % axis ang) (identity-mesh :norms))
              new-code (get-code-symmetry code i)]
          (println new-code)
          (swap! tileset-meshes assoc new-code {:tris new-tris :norms new-norms})
      )))
    (println "done"))
  )

(defn make-tileset-meshes-p [isolevel tileset xdim ydim zdim ]
  (println "make-tileset-meshes" isolevel tileset xdim ydim zdim)
  (reset! tileset-meshes {})
  (into {} (pmap #(vec [% (doall (make-tilecode-bezier-blob-surface isolevel % xdim ydim zdim))]) tileset))
;    (print code "...")
;    (swap! tileset-meshes assoc code (make-tilecode-bezier-blob-surface isolevel code xdim ydim zdim))
;    (println "done"))
  )

(defn cancel-surface-thread []
  (when (future? @surface-thread)
    (future-cancel @surface-thread)
    (if (or (future-cancelled? @surface-thread)
            (future-done? @surface-thread))
      (println "cancel-surface-thread failed"))))


(defn run-surface-thread [ts]
  (let [s (make-tilecode-bezier-blob-surface 0.125
                                             ((ts :tiles) [0 0 0])
                                             32 32 32)]
    (reset! test-surface s)
    ))


(defn start-surface-thread []
  (cancel-surface-thread)
  (reset! surface-thread (future (run-surface-thread)))
  )



;(defn get-t-for-bezier-point [p ps]
;  (let [t (->> ps
;               (map-indexed #(vec [(/ %1 (count ps)) %2]))
;               (filter #(= (second %) p))
;               (first)
;               (first))]
;    (if (nil? t) 0.0 (double t))))
;
;
;(defn bezier-blob [xyz f1-idx f2-idx r1 r2]
;  (let [curve-points (map #(vec3-scale % (topo-coord-scales (@current-topology :id)))
;                          (get-points-for-curve f1-idx f2-idx 10))
;        [closest-p closest-d n] (find-closest-point xyz curve-points)
;        t-at-closest-p (get-t-for-bezier-point closest-p curve-points)
;        radius-at-p (lerp- t-at-closest-p r1 r2)
;        ]
;    (if (and (< closest-d radius-at-p)
;             (polyhedron-contains? xyz (@current-topology :face-centers)))
;      [0.0 (n 0) (n 1) (n 2)]
;      [1.0 (n 0) (n 1) (n 2)])))
