(ns rhombrick.vector)

; 2d vector ops
(defn to-vec2 [angle]
  (let [rad (Math/toRadians angle)]
    [(Math/cos rad)
     (Math/sin rad)]))

(defn vec2-add [v1 v2]
  [(+ (v1 0) (v2 0))
   (+ (v1 1) (v2 1))])

(defn vec2-sub [v1 v2]
  [(- (v1 0) (v2 0))
   (- (v1 1) (v2 1))])

(defn vec2-scale [v s]
  [(* (v 0) s) (* (v 1) s)])

(defn vec2-length [[x y]] 
  (Math/sqrt (+ (* x x) (* y y))))

(defn vec2-normalize [[x y]]
  (let [l (vec2-length [x y])]
     (if (zero? l)
        [(/ x 0.000000001) (/ y 0.000000001)]
        [(/ x l) (/ y l)])))


(defn vec3-add [[v1x v1y v1z] [v2x v2y v2z]]
  [(+ v1x v2x) (+ v1y v2y) (+ v1z v2z)])

;(defn vec3-add [v1 v2]
;  [(+ (v1 0) (v2 0))
;   (+ (v1 1) (v2 1))
;   (+ (v1 2) (v2 2))])

;(defn vec3-add [v1 v2]
;  (vec (map #(+ %1 %2) v1 v2)))

(defn vec3-sub [v1 v2]
  [(- (v1 0) (v2 0))
   (- (v1 1) (v2 1))
   (- (v1 2) (v2 2))])

(defn vec3-mul [v1 v2]
  [(* (v1 0) (v2 0))
   (* (v1 1) (v2 1))
   (* (v1 2) (v2 2))])

(defn vec3-scale [v s]
  [(* (v 0) s) (* (v 1) s) (* (v 2) s)])

(defn vec3-sum-of-squares [v]
  (+ (* (v 0) (v 0)) (* (v 1) (v 1)) (* (v 2) (v 2))))

;(defn vec3-sum-of-squares [[x y z]]
;  (+ (* x x) (* y y) (* z z)))

(defn vec3-length [[x y z]]
  (Math/sqrt (+ (* x x) (* y y) (* z z))))

(defn vec3-normalize [[x y z]]
  (let [l (vec3-length [x y z])]
     (if (zero? l)
        [(/ x 0.000000001) (/ y 0.000000001) (/ z 0.000000001)]
        [(/ x l) (/ y l) (/ z l)])))


(defn vec3-cross [[v1x v1y v1z] [v2x v2y v2z]]
  [ (- (* v1y v2z) (* v1z v2y))
    (- (* v1z v2x) (* v1x v2z))
    (- (* v1x v2y) (* v1y v2x))])


(defn vec3-dot [[v1x v1y v1z] [v2x v2y v2z]]
  (+ (* v1x v2x) (* v1y v2y) (* v1z v2z)))


(defn vec3-angle-between [v1 v2]
  (let [l1 (vec3-length v1)
        l2 (vec3-length v2)
        dp (vec3-dot v1 v2)
        rads (Math/acos (/ dp (* l1 l2)))
        degs (* (/ rads Math/PI) 180.0) ]
    degs))


(defn vec3-distance [p1 p2]
  (vec3-length (vec3-sub p2 p1)))

(defn vec3-distance-squared [p1 p2]
  (vec3-sum-of-squares (vec3-sub p2 p1)))


(defn vec3-bisect [p1 p2]
  (vec3-scale (vec3-add p1 p2) 0.5))


(defn vec3-abs [v] [(Math/abs (v 0)) (Math/abs (v 1)) (Math/abs (v 2))])

(defn vec3-max [v1 v2] [(max (v1 0) (v2 0)) (max (v1 1) (v2 1)) (max (v1 2) (v2 2))])

(defn vec3-min [v1 v2] [(min (v1 0) (v2 0)) (min (v1 1) (v2 1)) (min (v1 2) (v2 2))])

(defn myround [s n]
  (double (.setScale (bigdec n) s java.math.RoundingMode/HALF_EVEN)))


(defn eps-to-zero [n e]
  (if (< (Math/abs n) e) 0.0 n))

(defn vec3-eps-to-zero [v e]
  [(eps-to-zero (v 0) e)
   (eps-to-zero (v 1) e)
   (eps-to-zero (v 2) e)])

;(defn vec3-quantize [v p]
;  [(myround p (v 0))
;   (myround p (v 1))
;   (myround p (v 2))])

(def vec3-quantize (memoize (fn [v p]
  [(myround p (v 0))
   (myround p (v 1))
   (myround p (v 2))])))

(defn rotate-point [point axis ang]
  (let [[x y z] point
        [u v w] axis
        ux (* u x) uy (* u y) uz (* u z)
        vx (* v x) vy (* v y) vz (* v z)
        wx (* w x) wy (* w y) wz (* w z)
        sa (Math/sin ang)
        ca (Math/cos ang)
        xn (+ (* u (+ ux vy wz))
              (* ca (- (* x (+ (* v v) (* w w)))
                       (* u (+ vy wz))))
              (* sa (+ (- wy) vz)))
        yn (+ (* v (+ ux vy wz))
              (* ca (- (* y (+ (* u u) (* w w)))
                       (* v (+ ux wz))))
              (* sa (- wx uz)))
        zn (+ (* w (+ ux vy wz))
              (* ca (- (* z (+ (* u u) (* v v)))
                       (* w (+ ux vy))))
              (* sa (+ (- vx) uy))) ]
    [xn yn zn]))



; p=point, pv=point on plane, pn=plane normal
(defn project-point-to-plane [p pv pn]
  (let [;norm (fn [v] (Math/sqrt (vec3-dot v v)))
        ;dist (fn [p q] (norm (vec3-sub p q)))
        sn (- (vec3-dot pn (vec3-sub p pv)))
        sd (vec3-dot pn pn)
        sb (/ sn sd)
        b (vec3-add p (vec3-scale pn sb))]
    b))

;; p=point, pv=point on plane, pn=plane normal
(defn distance-point-to-plane [p pv pn]
  (vec3-distance p (project-point-to-plane p pv pn)))


(defn signed-distance-point-to-plane [p pv pn]
  (let[prj-p (project-point-to-plane p pv pn)
       dir (vec3-sub p prj-p)
       n (vec3-normalize dir)
       dist (vec3-length dir)]
    (if (>= (vec3-dot n (vec3-normalize p)) 0.0)
      dist
      (- dist))))



;; p=point, pv=point on plane, pn=plane normal
;(defn distance-point-to-plane [p pv pn]
;  (let [norm (fn [v] (Math/sqrt (vec3-dot v v)))
;        dist (fn [p q] (norm (vec3-sub p q)))
;        sn (- (vec3-dot pn (vec3-sub p pv)))
;        sd (vec3-dot pn pn)
;        sb (/ sn sd)
;        b (vec3-add p (vec3-scale pn sb))]
;    (dist p b)))
