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
(defn polyhedron-contains? [xyz face-centers]
  (let [closest-f (first (sort-by #(vec3-distance xyz %) face-centers)) ]
    (if (= (count face-centers)
           (count (filter #(> % 0) (map #(is-below xyz (vec3-scale % 0.5))
                                        face-centers))))
      true
      false)))
      ;(vec (flatten [0.0 (vec3-normalize closest-f)]))
      ;(vec (flatten [1.0 (vec3-normalize closest-f)])))))



(defn get-points-for-curve [f1-idx f2-idx npoints]
  (let [step (/ 1 npoints)]
    (vec (map #(get-bezier-point-3d f1-idx f2-idx (* % step))
              (range npoints)))))


; find the closest ps to p. returns [closest distance]
(defn find-closest-point [p ps & best]
  (if (zero? (count ps))
    best
    (let [test-p (first ps)
          dist (vec3-distance p test-p)]
      (cond
        (nil? best)
          (recur p (vec (rest ps)) (vec [test-p dist [1 0 0]]))
        (< dist (best 1))
          (recur p (vec (rest ps)) (vec [test-p dist (vec3-normalize (vec3-sub p test-p))]))
        :else
          (recur p (vec (rest ps)) (vec best))))))


(defn get-t-for-bezier-point [p ps]
  (let [t (->> ps
               (map-indexed #(vec [(/ %1 (count ps)) %2]))
               (filter #(= (second %) p))
               (first)
               (first))]
    (if (nil? t) 0.0 (double t))))

  
;  (let [t (first (filter #(= %2 p) %1) ps)) ]
;    (if (nil? t)
;      0.0
;      (/ t (count ps)))))
(def topo-coord-scales {:square 1.0
                        :cube 2.0
                       :hexagon 1.0
                       :rhombic-dodecahedron 0.5
                       :truncated-octahedron 0.5})

(defn bezier-blob [xyz f1-idx f2-idx r1 r2]
  (let [curve-points (map #(vec3-scale % (topo-coord-scales (@current-topology :id)))
                          (get-points-for-curve f1-idx f2-idx 10))
        [closest-p closest-d n] (find-closest-point xyz curve-points)
        t-at-closest-p (get-t-for-bezier-point closest-p curve-points)
        radius-at-p (lerp- t-at-closest-p r1 r2)
        ]
    (if (and (< closest-d radius-at-p)
             (polyhedron-contains? xyz (@current-topology :face-centers)))
      [0.0 (n 0) (n 1) (n 2)]
      [1.0 (n 0) (n 1) (n 2)])))


;(defn tilecode-bezier-blob [xyz code]
;  (let [endpoint-pairs (make-curve-endpoints (get-connected-idxs code))]
;
;  ))

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


(defn make-surface [isolevel xdim ydim zdim]
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
                                                   [n _ _ _] (bezier-blob v 0 3 0.146875 0.4)]
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
                                      (let [[_ nx ny nz] (bezier-blob v 0 3 0.146875 0.4)]
                                      ;(let [[_ nx ny nz] (spheres-func v 0.4)]
                                        (normalize nx ny nz)))
                                    tris)]
                     
                     {:tris tris :norms norms}))
                 
                 grid-indices)]

    {:tris (mapcat :tris surface)
     :norms (mapcat :norms surface)}))


