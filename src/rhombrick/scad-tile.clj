;(defn get-curve-points [f1-idx f2-idx npoints topo]
;  (let [step (double (/ 1.0 npoints))]
;    (->> (map #(get-bezier-point-3d f1-idx f2-idx (* % step)) (range (inc npoints)))
;         vec)))


;(defn get-curve-data [code f1-idx f2-idx npoints topo]
;  (let [r1 (bbox/bezier-box-thicknesses (.charAt code f1-idx))
;        r2 (bbox/bezier-box-thicknesses (.charAt code f2-idx))
;        step (/ 1.0 npoints)
;        points (get-curve-points f1-idx f2-idx npoints topo)
;        radii (vec (map #(lerp- (* % step) r1 r2) (range (inc npoints)))) ]
;    {:points points
;     :radii radii}))

; (get-curve-data "1-2-1-" 0 2 4 (topologies :hexagon))


;(defn tilecode-bezier-blob-scad [code topo res sc]
;  (let [endpoint-pairs (vec (bbox/-make-curve-endpoints (get-connected-idxs code)))
;        curves-data (map #(get-curve-data code (% 0) (% 1) res topo)
;                         endpoint-pairs)
;        sphere-fn (fn [p rad] (translate (vec3-scale p sc)
;                              (sphere (* rad sc))))]
;    (union
;      (map #(map sphere-fn (% :points)
;                           (% :radii))
;           curves-data))))


;; (defn bezier-tube-test2 []
;;   (let [r1 10
;;         r2 75
;;         npoints 50
;;         controls1 [[0 -100 0] [ 100 -300 100] [300 0 100] [100 100 200]]
;;         controls2 [[0  100 0] [ 0  100 -200] [-200 -300 0] [-100 -100 200]]]
;;     ;(union
;;       ;(union
;;        (color [1 0 0 1]
;;         (bezier-tube controls1 r1 r2 npoints)
;;         ;(translate (controls1 0) (sphere 20))
;;         ;(translate (controls1 1) (sphere 20))
;;         ;(translate (controls1 2) (sphere 20))
;;         ;(translate (controls1 3) (sphere 20))
;;        )
;;        ;)

;;       ;(union
;;       ; (color [0 0 1 1]
;;       ;  (bezier-tube controls2 r1 r2 npoints)
;;       ;  (translate (controls2 0) (sphere 20))
;;       ;  (translate (controls2 1) (sphere 20))
;;       ;  (translate (controls2 2) (sphere 20))
;;       ;  (translate (controls2 3) (sphere 20))
;;       ; ))
;;       ;)
;;       ))




;; (def tile-scale 10)

;; (def scad-tile
;;   (intersection
;;     (scale [tile-scale tile-scale tile-scale] (polyhedron geom/rd-verts geom/rd-faces :convexity 8))
;;     (tilecode-bezier-blob-scad "-1---2---3-3"
;;                                  (topologies :rhombic-dodecahedron)
;;                                  16 ; res
;;                                  tile-scale)
;;    )
;;   )


(defn nonconnected-spheres [code topo sc]
  (union
    (->> (get-non-connected-idxs code)
         (map #((topo :face-centers) %))
         (map #(vec3-scale % sc))
         (map #(vec3-scale % 2.2))
         (map #(translate % (sphere 200))))))

(defn connected-spheres [code topo sc]
  (color [1 0 0 1]
  (union
    (->> (get-connected-idxs code)
         (map #((topo :face-centers) %))
         (map #(vec3-scale % sc))
         ;(map #(vec3-scale % 2.2))
         (map #(translate % (sphere 20))))))
)

(defn carved-tile [code topo sc]
  (difference
    (union
      (scale [sc sc sc] (polyhedron geom/rd-verts geom/rd-faces :convexity 8))
      (connected-spheres code topo sc))

    (nonconnected-spheres code topo sc)
    )
  )

  ;(spit "rhombrick.scad" (write-scad (carved-tile "1--1--1--1--"
  ;                                                (geom/topologies :rhombic-dodecahedron)
  ;                                                100)))


