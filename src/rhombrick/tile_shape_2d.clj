(ns rhombrick.tile-shape-2d
  (:use [rhombrick.vector]
        [rhombrick.staticgeometry]
        [rhombrick.bezierbox]
        [rhombrick.tilecode]))


(def shape-2d-cache (atom {}))
(def inner-shape-2d-cache (atom {}))

(defn shape-2d-cache-reset []
  (reset! shape-2d-cache {})
  (reset! inner-shape-2d-cache {}))


(def shape-2d-thicknesses   { ;\1 0.333
                              \1 0.25
                              \2 0.333
                              \3 0.5
                              \4 0.75
                              \5 0.8
                              \6 1.6875
                              \7 2.0
                             \a 0.25
                             \A 0.25
                             \b 0.333
                             \B 0.333
                             \c 0.5
                             \C 0.5
                             \d 0.75
                             \D 0.75
                             })

(def like-compatible-connector
  ;(vec (reverse
  [
   ;[0.0 1.0 0.0]
   [0.0 0.5 0.0]
   [-1.0 0.5 0.0]
   [-1.0 -0.5 0.0]
   [-0.5 -0.5 0.0]
   [-0.5 0.0 0.0]
   [0.5 0.0 0.0]
   [0.5 0.5 0.0]
   [1.0 0.5 0.0]
   [1.0 -0.5 0.0]
   [0.0 -0.5 0.0]
   ;[0.0 -1.0 0.0]
   ])
;))

(def tol jigsaw-tolerance)

;(defn apply-tolerance [verts]
;  (vec (map #(vec3-add (vec3-scale (vec3-normalize %) (- tol)) %) verts)))


(def like-compatible-tolerances
  [
   [(- tol) tol 0.0]
   [(- tol) tol 0.0]
   [(- tol) (- tol) 0.0]
   [tol (- tol) 0.0]
   [tol (- tol) 0.0]
   [tol (- tol) 0.0]
   [tol (- tol) 0.0]
   [(- tol) (- tol) 0.0]
   [(- tol) tol 0.0]
   [(- tol) tol 0.0]
   ])

(def yang-tolerances
  [
   [(- tol) (- tol) 0.0]
   [tol (- tol) 0.0]
   [tol (- tol) 0.0]
   [(- tol) (- tol) 0.0]
   [(- tol) tol 0.0]
   [tol tol 0.0]
   [tol tol 0.0]
   [(- tol) tol 0.0]
   ])

(def yin-tolerances
  (vec (map #(vec3-scale % 1.0)
    (reverse yang-tolerances))))


yin-tolerances

(def opposite-compatible-connector-yang
  [
   ;[0.0 1.0 0.0]
   [0.0 0.25 0.0]
   [0.5 0.25 0.0]
   [0.5 0.5 0.0]
   [1.0 0.5 0.0]
   [1.0 -0.5 0.0]
   [0.5 -0.5 0.0]
   [0.5 -0.25 0.0]
   [0.0 -0.25 0.0]
   ;[0.0 -1.0 0.0]
   ])

(def opposite-compatible-connector-yin
  (vec (map #(vec3-scale % -1.0)
    (reverse
     opposite-compatible-connector-yang))))


(defn get-connector-verts-for-digit [digit]
  (if (face-digit-like-compatible? digit)
    like-compatible-connector
    ;(vec (map vec3-add like-compatible-connector like-compatible-tolerances))
    (if (contains? #{\a \b \c \d} digit)
      opposite-compatible-connector-yin
      opposite-compatible-connector-yang
      ;(vec (map vec3-add opposite-compatible-connector-yin yin-tolerances))
      ;(vec (map vec3-add opposite-compatible-connector-yang yang-tolerances))
      )))


(defn get-connector-verts-for-digit-orig [digit]
  (if (face-digit-like-compatible? digit)
    (vec (map vec3-add like-compatible-connector like-compatible-tolerances))
    (if (contains? #{\a \b \c \d} digit)
      (vec (map vec3-add opposite-compatible-connector-yin yin-tolerances))
      (vec (map vec3-add opposite-compatible-connector-yang yang-tolerances))
      )))


(defn get-tolerance-offsets [digit]
  (if (face-digit-like-compatible? digit)
    like-compatible-tolerances
    (if (contains? #{\a \b \c \d} digit)
      yin-tolerances
      yang-tolerances)))


;(def get-bez-anchors-2d (memoize (fn [code topo]
(defn get-bez-anchor-offsets-2d [code topo]
  (let [vs (rotate-vec (topo :verts-2d))
;  (let [vs (rotate-vec (apply-tolerance(topo :verts-2d)))
        rads (->> (map shape-2d-thicknesses code)
                  (filter #(not (nil? %)))
                  (map #(* % 0.5)))
        dirs (map #(vec3-normalize
                     (vec3-sub (vs %)
                               (vs (mod (inc %) (topo :num-faces)))))
                  (get-connected-idxs code))]
    (->> (map vec3-scale dirs rads)
         (map #(vec [% (vec3-scale % -1.0)]))
         (vec))))


(defn get-shape-2d-curve-endpoints [con-idxs]
  (if (= (count con-idxs) 1)
    (list [(first con-idxs) (first con-idxs)])
    (map #(vec [(con-idxs %) (con-idxs (mod (inc %) (count con-idxs)))])
         (range (count con-idxs)))))


;(def make-facecode-shape-2d (memoize (fn [code topo res]
(defn make-facecode-shape-2d [code topo res]
  ;(println "make-facecode-shape-2d:" code (topo :id res))
  (let [con-idxs (vec (get-connected-idxs code))
        num-idxs (count con-idxs)
        offsets (get-bez-anchor-offsets-2d code topo)]
    (->> (get-shape-2d-curve-endpoints con-idxs)
         (map-indexed #(get-shape-2d-bezier-controls-with-offset-and-tolerance
                         (%2 0)
                         (%2 1)
                         ((offsets %1) 1)
                         ((offsets (mod (inc %1) num-idxs)) 0))) ; controls
         (map #(get-bezier-points % res))) ; curves verts
))


(defn make-facecode-inner-shape-2d [code topo res]
  ;(println "make-facecode-shape-2d:" code (topo :id res))
  (let [con-idxs (vec (get-connected-idxs code))
        num-idxs (count con-idxs)
        offsets (get-bez-anchor-offsets-2d code topo)]
    (->> (get-shape-2d-curve-endpoints con-idxs)
         (map-indexed #(get-shape-2d-bezier-controls-with-offset
                         (%2 0)
                         (%2 1)
                         [0 0 0] [0 0 0] ))
         (map #(get-bezier-points % res))) ; curves verts
))


;(defn rotate-connector [i]
;  (let [ang (* (- i) (/ (* 2.0 Math/PI) 6.0))]
;    (vec (map #(rotate-point % [0.0 0.0 1.0] ang)
;              like-compatible-connector))))

(defn rotate-connector [i conn-verts topo]
  (let [ang (* (- i) (/ (* 2.0 Math/PI) (topo :num-faces)))]
    (vec (map #(rotate-point % [0.0 0.0 1.0] ang)
              conn-verts))))

(defn translate-connector-verts [vs p]
  (vec (map #(vec3-add p %) vs)
  ))

(defn scale-connector-verts [vs s]
  (vec (map #(vec3-scale % s) vs)
  ))




(defn make-jigsaw-piece [code topo res]
  (let [curves (make-facecode-shape-2d code topo res)
        con-idxs (get-connected-idxs code)
        face-centers (into {} (map #(vec [% ((topo :face-centers) %)]) con-idxs))
        conns (into {} (vec (map #(vec [% (get-connector-verts-for-digit (.charAt code %))]) con-idxs)))
        tolerance-offsets (into {} (map #(vec [% (get-tolerance-offsets (.charAt code %))])
                                        con-idxs))
        conns-scaled (into {} (map #(vec [% (scale-connector-verts (conns %)
                                                                   (* 0.5 (shape-2d-thicknesses (.charAt code %)))                    )])
                                   con-idxs))
        conns-tol (into {} (map #(vec [% (map vec3-add (conns-scaled %) (tolerance-offsets %))])
                                con-idxs))
        conns-rotated (into {} (vec (map #(vec [% (rotate-connector % (conns-tol %) topo)]) con-idxs)))
        conns-tr (map #(translate-connector-verts (conns-rotated %) (face-centers %))
                      con-idxs)
        shape (vec (interleave  conns-tr curves))
        ]
    shape
    ))


(defn make-jigsaw-piece-orig [code topo res]
  (let [curves (make-facecode-shape-2d code topo res)
        con-idxs (get-connected-idxs code)
        face-centers (into {} (map #(vec [% ((topo :face-centers) %)]) con-idxs))
        ;conns-rotated (into {} (vec (map #(vec [% (rotate-connector %)]) con-idxs)))

        conns (into {} (vec (map #(vec [% (get-connector-verts-for-digit (.charAt code %))]) con-idxs)))

        tolerance-offsets (into {} (map #(vec [% (get-tolerance-offsets (.charAt code %))])
                                        con-idxs))

        conns-rotated (into {} (vec (map #(vec [% (rotate-connector % (conns %) topo)]) con-idxs)))

        conns-scaled (into {} (map #(vec [% (scale-connector-verts (conns-rotated %)
                                                                   (* 0.5 (shape-2d-thicknesses (.charAt code %)))
                                                                   )])
                                   con-idxs))

        conns-tr (map #(translate-connector-verts (conns-scaled %) (face-centers %))
                      con-idxs)
        shape (vec (interleave  conns-tr curves))
        ]
    ;(apply concat shape)
    shape
    ))



(defn get-facecode-shape-2d [code topo res]
  (if (contains? @shape-2d-cache [code topo res])
    (@shape-2d-cache [code topo res])
    (do
      (swap! shape-2d-cache assoc [code topo res]
             ;(make-facecode-shape-2d code topo res))
             (make-jigsaw-piece code topo res))
      (@shape-2d-cache [code topo res]))))


(defn get-facecode-inner-shape-2d [code topo res]
  (if (contains? @inner-shape-2d-cache [code topo res])
    (@inner-shape-2d-cache [code topo res])
    (do
      (swap! inner-shape-2d-cache assoc [code topo res]
             (make-facecode-inner-shape-2d code topo res))
      (@inner-shape-2d-cache [code topo res]))))












