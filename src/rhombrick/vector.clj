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


(defn vec3-add [v1 v2]
  [(+ (v1 0) (v2 0))
   (+ (v1 1) (v2 1))
   (+ (v1 2) (v2 2))])

;(defn vec3-add [v1 v2]
;  (vec (map #(+ %1 %2) v1 v2)))

(defn vec3-scale [v s]
  [(* (v 0) s) (* (v 1) s) (* (v 2) s)])



