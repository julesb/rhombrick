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

(defn vec3-scale [v s]
  [(* (v 0) s) (* (v 1) s) (* (v 2) s)])

(defn vec3-sum-of-squares [[x y z]]
  (+ (* x x) (* y y) (* z z)))

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

(defn vec3-bisect [p1 p2]
  (vec3-scale (vec3-add p1 p2) 0.5))


(defn myround [s n]
  (double (.setScale (bigdec n) s java.math.RoundingMode/HALF_EVEN)))


(defn eps-to-zero [n e]
  (if (< (Math/abs n) e) 0.0 n))

(defn vec3-eps-to-zero [v e]
  [(eps-to-zero (v 0) e)
   (eps-to-zero (v 1) e)
   (eps-to-zero (v 2) e)])

(defn vec3-quantize [v p]
  [(myround p (v 0))
   (myround p (v 1))
   (myround p (v 2))])


