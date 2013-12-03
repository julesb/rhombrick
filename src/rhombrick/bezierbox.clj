(ns rhombrick.bezierbox
  (:use [rhombrick.vector]
        [rhombrick.staticgeometry]
        [clojure.math.combinatorics]
    ))

(def bezier-box-tristrip-cache (atom {}))
(def bezier-box-tristrip-normals-cache (atom {}))
(def bezier-box-line-cache (atom {}))
(def bezier-box-resolution (atom 8))
(def bezier-box-control-bias (atom 0.5))


(def bezier-box-thicknesses { \1 0.146875
                              \2 0.29375
                              \3 0.5875
                              \4 1.175
                              \5 1.375
                              \6 1.6875
                              \7 2.0
                             \a 0.146875
                             \A 0.146875
                             \b 0.29375
                             \B 0.29375
                             \c 0.5875
                             \C 0.5875
                             \d 1.175
                             \D 1.175
                             })


(defn -get-num-connected [code]
  (count (filter #(and (not= \- %) (not= \0 %)) code)))


(defn get-bezier-anchor-offsets-rotated [f-idx]
  (let [o (bezier-anchor-offsets f-idx)]
    (vec [(vec3-bisect (o 0) (o 1)) 
              (vec3-bisect (o 1) (o 2))
              (vec3-bisect (o 2) (o 3))
              (vec3-bisect (o 3) (o 0))])))


; this is hideous and horrible but as long as it spits out the right numbers
; it will do, since it will only be used for precomputing triangle strip data 
(defn get-bezier-anchor-offsets [f1-idx f2-idx]
  (let [f1-center ((@current-topology :face-centers) f1-idx)
        f2-center ((@current-topology :face-centers) f2-idx)
        f1-f2-bisect (vec3-normalize (vec3-scale (vec3-add f1-center f2-center) 0.5))
        ;f1-f2-bisect (vec3-scale (vec3-add f1-center f2-center) 0.5)

        f1-offsets (get-bezier-anchor-offsets-rotated f1-idx)
        f2-offsets (get-bezier-anchor-offsets-rotated f2-idx)
        ;f1-offsets-sorted (sort-by #(vec3-distance f1-f2-bisect %) f1-offsets)
        ;f2-offsets-sorted (sort-by #(vec3-distance f1-f2-bisect %) f2-offsets)

        f1-off-0 (sort-by #(% 0) f1-offsets)
        f1-off-1 (sort-by #(% 1) f1-off-0)
        f1-off-2 (sort-by #(% 2) f1-off-1)
        f2-off-0 (sort-by #(% 0) f2-offsets)
        f2-off-1 (sort-by #(% 1) f2-off-0)
        f2-off-2 (sort-by #(% 2) f2-off-1)

        f1-offsets-sorted (vec (sort-by #(vec3-distance f1-f2-bisect %) f1-off-2))
        f2-offsets-sorted (vec (sort-by #(vec3-distance f1-f2-bisect %) f2-off-2))

        f1-test-vec (vec3-sub f1-center (f1-offsets-sorted 0))
        f2-test-vec (vec3-sub f2-center (f2-offsets-sorted 0))

        f1-offsets-sorted-by-angle (vec (sort-by #(+ 180.0 (vec3-angle-between f1-test-vec
                                                                               (vec3-sub f1-center % )))
                                                 f1-offsets-sorted))
        f2-offsets-sorted-by-angle (vec (sort-by #(+ 180.0 (vec3-angle-between f2-test-vec
                                                                               (vec3-sub f2-center % )))
                                                 f2-offsets-sorted))

        f1-offsets-swapped f1-offsets-sorted-by-angle
        f2-offsets-swapped (if (= 120 (int (Math/rint (get-angle-for-face-idxs [f1-idx f2-idx]))))
                             [(f2-offsets-sorted-by-angle 0)
                              (f2-offsets-sorted-by-angle 2)
                              (f2-offsets-sorted-by-angle 1)
                              (f2-offsets-sorted-by-angle 3)]
                              f2-offsets-sorted-by-angle)
        f1-offsets-swapped-2 [(f1-offsets-swapped 0)
                              (f1-offsets-swapped 1)
                              (f1-offsets-swapped  3)
                              (f1-offsets-swapped  2)]
        f2-offsets-swapped-2 [(f2-offsets-swapped  0)
                              (f2-offsets-swapped  1)
                              (f2-offsets-swapped  3)
                              (f2-offsets-swapped  2)]
        ]
    [f1-offsets-swapped-2 f2-offsets-swapped-2]
  ))


(defn get-bezier-controls [f1-idx f2-idx]
  "Given two face indices, returns a vec of four 3d bezier control points"
  (let [p1 ((@current-topology :face-centers) f1-idx)
        p2 (vec3-scale p1 @bezier-box-control-bias)
        p4 ((@current-topology :face-centers) f2-idx)
        p3 (vec3-scale p4 @bezier-box-control-bias)]
    (if (not= f1-idx f2-idx)
      [p1 p2 p3 p4]
      [p1 p2 p3 [0 0 0]])))


(defn get-bezier-controls-with-offset [f1-idx f2-idx f1-offset f2-offset]
  "Given two face indices, returns a vec of four 3d bezier control points"
  "offset is a vector to specify the offset for bezier boxes."
  (let [p1 (vec3-add ((@current-topology :face-centers) f1-idx) f1-offset)
        p2 (vec3-sub p1 (vec3-scale ((@current-topology :face-centers) f1-idx) @bezier-box-control-bias))
        p4 (vec3-add ((@current-topology :face-centers) f2-idx) f2-offset)
        p3 (vec3-sub p4 (vec3-scale ((@current-topology :face-centers) f2-idx) @bezier-box-control-bias))]
    (if (not= f1-idx f2-idx)
      [p1 p2 p3 p4]
      [p1 p2 p3 [0 0 0]])))


;;
;; From Processing source code:
;;  public float bezierPoint(float a, float b, float c, float d, float t) {
;;    float t1 = 1.0f - t;
;;    return a*t1*t1*t1 + 3*b*t*t1*t1 + 3*c*t*t*t1 + d*t*t*t;
;;  }
;;
;;  public float bezierTangent(float a, float b, float c, float d, float t) {
;;    return (3*t*t * (-a+3*b-3*c+d) +
;;            6*t * (a-2*b+c) +
;;            3 * (-a+b));
;;  }
;;
(defn bezier-point2 [a b c d t]
  (let [t1 (- 1.0 t)]
    (+ (* a t1 t1 t1)
       (* 3 b t t1 t1)
       (* 3 c t t t1)
       (* d t t t))))


(defn get-bezier-point-3d [f1-idx f2-idx t]
  (when (not= f1-idx f2-idx)
    (let [[p1 p2 p3 p4] (get-bezier-controls f1-idx f2-idx)
          bx (bezier-point2 (p1 0) (p2 0) (p3 0) (p4 0) t)
          by (bezier-point2 (p1 1) (p2 1) (p3 1) (p4 1) t)
          bz (bezier-point2 (p1 2) (p2 2) (p3 2) (p4 2) t)]
      [bx by bz])))


(defn get-bezier-point [[p1 p2 p3 p4] t]
  [ (bezier-point2 (p1 0) (p2 0) (p3 0) (p4 0) t)
    (bezier-point2 (p1 1) (p2 1) (p3 1) (p4 1) t)
    (bezier-point2 (p1 2) (p2 2) (p3 2) (p4 2) t)])


;(defn get-bezier-tangent-3d [f1-idx f2-idx t]
;  (when (not= f1-idx f2-idx)
;    (let [[p1 p2 p3 p4] (get-bezier-controls f1-idx f2-idx)
;          tx (bezier-tangent (p1 0) (p2 0) (p3 0) (p4 0) t)
;          ty (bezier-tangent (p1 1) (p2 1) (p3 1) (p4 1) t)
;          tz (bezier-tangent (p1 2) (p2 2) (p3 2) (p4 2) t)]
;      [tx ty tz])))


(defn -make-curve-endpoints [connected-idxs]
  (if (= (count connected-idxs) 1)
    (list [(first connected-idxs) (first connected-idxs)])
    (map vec (vec (combinations connected-idxs 2)))))


(defn bezier-box-tristrip-cache-reset []
  (reset! bezier-box-tristrip-cache {}))


(defn bezier-box-tristrip-normals-cache-reset []
  (reset! bezier-box-tristrip-normals-cache {}))


(defn bezier-box-line-cache-reset []
  (reset! bezier-box-line-cache {}))


(defn bezier-box-cache-reset []
  (bezier-box-tristrip-cache-reset)
  (bezier-box-tristrip-normals-cache-reset)
  (bezier-box-line-cache-reset))


(defn get-bezier-strip [c1 c2 steps]
  (map #(let [t (* % (/ 1 steps))]
         [(get-bezier-point c1 t)
          (get-bezier-point c2 t)])
       (range (inc steps))))


(defn get-face-normal [[v0 v1 v2]]
 (vec3-normalize (vec3-cross (vec3-sub v1 v0) (vec3-sub v2 v1))))


(defn alternate-winding [v idx]
  (if (= (mod idx 2) 1)
    [(v 1) (v 0) (v 2)]
    v))


(defn get-strip-face-normals [strip]
  (->> strip
       (partition 3 1)
       (map-indexed #(alternate-winding (vec %2) %1))
       (map get-face-normal)
       vec))


; Calculate the vertex normals, where each normal is the average of the normals
; of all triangles that the vertex is a part of.
(defn make-strip-vertex-normals [strip]
  (let [face-normals (get-strip-face-normals strip)
        n-faces (count face-normals)
        n-vnorms (+ n-faces 2)
        ; the first, second, second-last and last normals are special cases
        ; so calculate them separately
        vn0 (face-normals 0)
        vn1 (vec3-normalize (vec3-scale
                              (vec3-add (face-normals 0)
                                        (face-normals 1))
                              0.5))
        vnN-2 (vec3-normalize (vec3-scale
                                (vec3-add (face-normals (- n-faces 2))
                                          (face-normals (- n-faces 1)))
                                0.5))
        vnN-1 (face-normals (- n-faces 1))
        ; calculate all the rest of the normals
        vnorms (->> (range 2 (- n-vnorms 2))
                    (map #(vec3-scale
                            (vec3-add (vec3-add (face-normals (- % 2))
                                                (face-normals (- % 1)))
                                      (face-normals %))
                            0.33333333333))
                    (map vec3-normalize)
                    vec)]
    ; and concatenate them all into a single vector
    (vec (concat [vn0 vn1] vnorms [vnN-2 vnN-1]))))


(defn get-bezier-points [c1 steps]
  (map #(let [t (* % (/ 1 steps))]
          (get-bezier-point c1 t))
       (range (inc steps))))


(defn make-bezier-box-triangles [f1-idx f2-idx f1-scale f2-scale steps]
  (let [[f1-off f2-off] (get-bezier-anchor-offsets f1-idx f2-idx)
        f1-offsets (map #(vec3-scale % f1-scale) f1-off)
        f2-offsets (map #(vec3-scale % f2-scale) f2-off)
        controls (vec (map #(get-bezier-controls-with-offset f1-idx f2-idx %1 %2)
                       f1-offsets f2-offsets))]
    (map #(let [c1-idx %
                c2-idx (mod (inc c1-idx) 4)
                c1 (controls c1-idx)
                c2 (controls c2-idx)]
            (apply concat (get-bezier-strip c1 c2 steps)))
         (range (count controls)))))


(defn make-bezier-box-lines [f1-idx f2-idx f1-scale f2-scale steps]
  (let [[f1-off f2-off] (get-bezier-anchor-offsets f1-idx f2-idx)
        f1-offsets (map #(vec3-scale % f1-scale) f1-off)
        f2-offsets (map #(vec3-scale % f2-scale) f2-off)
        controls (vec (map #(get-bezier-controls-with-offset f1-idx f2-idx %1 %2)
                       f1-offsets f2-offsets))]
    (map #(let [c1 (controls %)]
            (get-bezier-points c1 steps))
            ;(apply concat (get-bezier-points c1 steps)))
         (range (count controls)))))


(defn make-facecode-bezier-box-triangles [^String code steps]
  (let [num-connected (-get-num-connected code)
        endpoint-pairs (if (< num-connected 4)
                         (vec (-make-curve-endpoints (get-connected-idxs code)))
                         (vec (filter #(not= 6 (Math/abs (- (% 1) (% 0))))
                                      (-make-curve-endpoints (get-connected-idxs code)))))]
    (map #(let [f1-thickness (bezier-box-thicknesses (.charAt code (% 0)))
                f2-thickness (bezier-box-thicknesses (.charAt code (% 1)))]
            (make-bezier-box-triangles (% 0) (% 1) f1-thickness f2-thickness steps))
         endpoint-pairs)))


(defn make-facecode-bezier-box-lines [^String code steps]
  (let [num-connected (-get-num-connected code)
        endpoint-pairs (if (< num-connected 4)
                         (vec (-make-curve-endpoints (get-connected-idxs code)))
                         (vec (filter #(not= 6 (Math/abs (- (% 1) (% 0))))
                                      (-make-curve-endpoints (get-connected-idxs code)))))]
    (map #(let [f1-thickness (bezier-box-thicknesses (.charAt code (% 0)))
                f2-thickness (bezier-box-thicknesses (.charAt code (% 1)))]
            (make-bezier-box-lines (% 0) (% 1) f1-thickness f2-thickness steps))
         endpoint-pairs)))


(defn get-bezier-box-triangles [code steps]
  "Gets bezier box triangle strips from cache if present, otherwise computes "
  "the triangle strips, adds them to the cache, then returns them."
  (when (not (contains? @bezier-box-tristrip-cache code))
    (let [strips (make-facecode-bezier-box-triangles code steps)]
      (swap! bezier-box-tristrip-cache assoc code strips)))
  (@bezier-box-tristrip-cache code))


(defn get-bezier-box-normals [code steps]
  (when (not (contains? @bezier-box-tristrip-cache code))
    (let [strips (first (make-facecode-bezier-box-triangles code steps))
          normals (map make-strip-vertex-normals strips)]
      (swap! bezier-box-tristrip-cache assoc code strips)
      (swap! bezier-box-tristrip-normals-cache assoc code normals)))
  (@bezier-box-tristrip-normals-cache code))


(defn get-bezier-box-lines [code steps]
  "Gets bezier box outline verts from cache if present, otherwise computes "
  "the verts, adds them to the cache, then returns them."
  (when (not (contains? @bezier-box-line-cache code))
    (let [lines (make-facecode-bezier-box-lines code steps)]
      (swap! bezier-box-line-cache assoc code lines)))
  (@bezier-box-line-cache code))

