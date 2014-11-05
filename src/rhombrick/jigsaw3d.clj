(ns rhombrick.jigsaw3d
  (:refer-clojure :exclude [import use])
  (:use [scad-clj.scad]
        [scad-clj.model]
        [rhombrick.tiling]
        [rhombrick.staticgeometry :as geom]
        [rhombrick.vector]
        ))


(def connector-lc
  (let [rad 10
        pin-rad 2.5
        pin-off 5]
    (rotate (/ Math/PI 4.0) [0 0 1]
      (difference
        ;(union
          (translate [0 0 (- (/ rad 2.0))]
            (cylinder rad rad))
          (translate [0 (- pin-off) 2.5]
            (cylinder pin-rad 5.1))
          (translate [0 pin-off 2.5]
            (cylinder pin-rad 5.1))
         ; )
        (translate [pin-off 0 -2.5]
            (cylinder pin-rad 5.1))
        (translate [(- pin-off) 0 -2.5]
          (cylinder pin-rad 5.1))))))

(def connector-oc-yin
  (let [rad 10
        pin-rad 3.3333
        pin-off 4.5]
    (union
      (translate [0 0 (- (/ rad 2.0))]
        (cylinder rad rad))
      (translate [0 (- pin-off) 2.5]
        (cylinder pin-rad 5.1))
      (translate [0 pin-off 2.5]
        (cylinder pin-rad 5.1)))))


(def connector-oc-yang
  (let [rad 10
        pin-rad 3.3333
        pin-off 4.5]
    (difference
      (translate [0 0 (- (/ rad 2.0))]
        (cylinder rad rad))
      (translate [0 (- pin-off) -2.5]
          (cylinder pin-rad 5.1))
      (translate [0 pin-off -2.5]
        (cylinder pin-rad 5.1)))))

(def connectors-test
  (union
   (translate [-25 0 0] connector-lc)
   (translate [0 0 0] connector-oc-yin)
   (translate [25 0 0] connector-oc-yang)
   (translate [0 0 -9.5] (cube 50 1 1))))



(defn vert-has-connected-faces? [vert-idx code]
  (let[con-idxs (into #{} (get-connected-idxs code))
       face-idxs (into #{} (rd-vert-face-map vert-idx))]
    (> (count (clojure.set/intersection con-idxs face-idxs)) 0)))


(defn carve-verts [code]
  (union (->> (range (count rd-verts))
              (filter #(not (vert-has-connected-faces? % code)))
              (map rd-verts)
              (map #(translate % (with-fs 0.1 (sphere 1.25)))))))


(defn get-connector [f-idx code topo]
  (let [pos ((topo :face-centers) f-idx)
        axis (vec3-normalize pos)
        az (Math/atan2 (axis 1) (axis 0))
        el (- (Math/asin (axis 2)))]
    (translate pos (with-fs 0.1 (sphere 0.25)))
  ))


(defn add-connectors [obj code topo]
  (let [con-idxs (get-connected-idxs code)
        connectors (union (map #(get-connector % code topo) con-idxs))
        ]
    (union obj connectors)
    )
  )

(defn make-carved-tile [code topo]
  (let [sphere-positions (->> (get-non-connected-idxs code)
                              (map #((topo :face-centers) %))
                              (map #(vec3-scale % 2.0)))]
    (difference
     (polyhedron geom/rd-verts geom/rd-faces :convexity 8)
     (union (map #(translate % (with-fs 0.1 (sphere 1.75))) sphere-positions))
     (union (map #(translate % (with-fs 0.1 (sphere 0.75))) (topo :verts)))
     (carve-verts code)
     )))


(defn do-tiling  [ts topo]
  (let [tiles (ts :tiles)
        tile-fn (fn [pos code]
                  (translate (vec3-scale pos 1.01)
                    (scale [1 1 1]
                      (make-carved-tile code topo))))]
    (union (map #(tile-fn (key %) (val %)) tiles))))



(def code "---b13B-----")
(def topo (geom/topologies :rhombic-dodecahedron))

(def carved-tile (add-connectors (make-carved-tile code topo)
                                 code topo))

;(def carved-tiling (do-tiling @tiler-state @current-topology))

(spit "rhombrick-carved.scad" (write-scad carved-tile))
;(spit "rhombrick-carved-tiling.scad" (write-scad carved-tiling))
(spit "connectors-test.scad" (write-scad connectors-test))


