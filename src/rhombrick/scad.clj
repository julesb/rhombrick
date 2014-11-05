(ns rhombrick.scad
  (:use [scad-clj.scad]
        [scad-clj.model]
        [rhombrick.staticgeometry :as geom]
        [rhombrick.bezierbox :as bbox]
        [rhombrick.marching-cubes :as mc]
        [rhombrick.vector]
        [clojure.string :only [join]]
        ))


; polyhedron() wasnt implemented in scad-clj - patch has been merged
(defmethod write-expr :polyhedron [depth [form {:keys [points faces convexity]}]]
  `(~@(indent depth) "polyhedron ("
    "points=[[" ~(join "], [" (map #(join ", " %1) points)) "]], "
    "faces=[[" ~(join "], [" (map #(join ", " %1) faces)) "]]"
    ~@(if (nil? convexity) [] [", convexity=" convexity])
    ");\n"))

(defn polyhedron
  ([points faces]
    `(:polyhedron {:points ~points :faces ~faces}))
  ([points faces & {:keys [convexity]}]
    `(:polyhedron {:points ~points :faces ~faces :convexity ~convexity})))

; ^^ polyhedron stuff can be removed after next scad-clj release on clojars ^^
;


(reset! geom/current-topology (geom/topologies :rhombic-dodecahedron))


(defn bezier-tube [[p1 p2 p3 p4] r1 r2 res]
  (let [points (bbox/get-bezier-points [p1 p2 p3 p4] res)
        radii (map #(mc/lerp- (* % (/ 1.0 res)) r1 r2) (range (inc res)))
        sphere-fn (fn [p rad] (translate p (sphere rad)))]
    (union (map sphere-fn points radii))))


(defn bezier-tube-test []
  (let [controls [[0 -100 0] [ 100 -300 100] [300 0 100] [100 100 200]]
        r1 20
        r2 50
        npoints 100]
    (bezier-tube controls r1 r2 npoints)))



(def primitives
  (difference
    (intersection
      (scale [10 10 10] (polyhedron geom/rd-verts geom/rd-faces :convexity 8))
      (sphere 17 ))
    (map #(translate (vec3-scale % 10) (sphere 2))
         ((geom/topologies :rhombic-dodecahedron) :face-centers))
  )
)



(comment

;(def mesh (->> (mc/make-tilecode-surface 0.0 "-2---2-----1-3" 32 32 32)
(def mesh (->> (mc/make-tilecode-surface 0.0 "---b13B-----" 24 24 24)
               :tris
               (partition 3)
               vec
               (#(mc/make-indexed-faces % [] []))))

(def tile-surface ;(intersection
                    (scale [50 50 50]
                      (polyhedron (mesh :verts) (mesh :faces)))
                    ;(scale [25 25 25]
                    ;  (polyhedron geom/rd-verts geom/rd-faces :convexity 8))
                    ;(with-fa 0.01 (sphere 40))

                   ;)
  )
)






;(def tile
;  (intersection
;    tile-surface
;    (sphere 40)
;   )
;  )

; (count (mesh :verts))
; (count (mesh :faces))


;(spit "rhombrick.scad" (write-scad tile-surface))
;(spit "rhombrick-carved.scad" (write-scad carved-tile))

; (spit "rhombrick.scad" (write-scad primitives))
; (spit "rhombrick.scad" (write-scad (bezier-tube-test)))


