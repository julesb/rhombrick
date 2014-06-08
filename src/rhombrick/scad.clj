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



;(spit "rhombrick.scad" (write-scad primitives))

;(spit "rhombrick.scad" (write-scad (bezier-tube-test)))


