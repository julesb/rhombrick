(ns rhombrick.tiling-render
  (:use [quil.core]
        [rhombrick.vector]
        [rhombrick.staticgeometry]
        [rhombrick.facecode]
        [rhombrick.tiling]))


; _______________________________________________________________________

(defn draw-rhomb-verts []
  (stroke 0 0 255 255)
  (doseq [v rd-verts]
    (point (v 0) (v 1) (v 2))
    ))

; _______________________________________________________________________

(defn draw-verts [vert-array]
  (doseq [v vert-array]
    (point (v 0) (v 1) (v 2))
    ))

; _______________________________________________________________________



(defn draw-faces [verts faces colors]
  (doseq [i (range (count faces))]
      (let [vert-idx (faces i)
            v0 (verts (vert-idx 0))
            v1 (verts (vert-idx 1))
            v2 (verts (vert-idx 2))
            v3 (verts (vert-idx 3))
            col (if (seq colors) (colors i) 0)]
        
        ;(no-fill)
        ;(stroke 0 0 255 128)
        ;(fill 0 0 0 32)
        ;(no-stroke)
        (if (seq colors)
          (fill (/ (col 0) 3.0) (/ (col 1) 3.0) (/ (col 2) 3.0) 128 ))
        (begin-shape :quads)
        (vertex (v0 0) (v0 1) (v0 2))
        (vertex (v1 0) (v1 1) (v1 2))
        (vertex (v2 0) (v2 1) (v2 2))
        (vertex (v3 0) (v3 1) (v3 2))
        (end-shape))))


; _______________________________________________________________________


(defn draw-todo []
  (doseq [tile  @todo]
    (let [pos tile]
      (with-translation pos 
        (push-matrix)
        (scale 0.5)
        (box 1 1 1)
        ;(draw-faces rd-verts rd-faces rd-face-colors)
        (pop-matrix)))))

; _______________________________________________________________________


(defn draw-todo-head []
  ;(fill 255 255 0 255)
  (stroke 64 64 255 220)
  (stroke-weight 8)
  (if (not (empty? @todo)) 
    (with-translation (peek @todo)
      (push-matrix)
      (scale 0.45)
      (draw-faces rd-verts rd-faces rd-face-colors)
      (pop-matrix))))
    ;(sphere 1)))

; _______________________________________________________________________



(defn draw-curve [f1-idx f2-idx]
  (if (not= f1-idx f2-idx)
  (let [p1 (co-verts f1-idx)
        p2 (vec3-scale p1 0.5)
        p4 (co-verts f2-idx)
        p3 (vec3-scale p4 0.5)]
    (bezier (p1 0) (p1 1) (p1 2)
            (p2 0) (p2 1) (p2 2)
            (p3 0) (p3 1) (p3 2)
            (p4 0) (p4 1) (p4 2)))))

; _______________________________________________________________________



;(defn get-connected-idxs [facecode]
;  (filter #(not= nil %)
;          (map #(if (= %2 \1) %1 nil)
;               (range 12) facecode)))

; buggy:
;(defn get-connected-idxs [facecode]
;  (filter #(not= nil %)
;          (map #(if (not= %2 \0) %1 nil)
;               (range 12) facecode)))

(defn make-curve-endpoints [connected-idxs]
  (let [num-points (count connected-idxs)]
    (map #(vector %1 (nth connected-idxs (mod (+ %2 1) num-points)))
         connected-idxs (range num-points))))

; _______________________________________________________________________

(defn draw-facecode [code]
  (let [endpoint-pairs (make-curve-endpoints (get-connected-idxs code))
        num-connected (count (filter #(= \1 %) code))]
    (if (= code "xxxxxxxxxxxx")
      (do 
        (no-stroke)
        (fill 255 0 0 64)
        (sphere 0.25)
        (no-fill)))
    (if (= num-connected 1)
      (let [p (co-verts (first (get-connected-idxs code)))]
        (stroke 192 192 255 192)
        (line 0 0 0 (p 0) (p 1) (p 2))
        (no-stroke)
        (fill 255 255 255 255)
        (sphere 0.125)
        (no-fill))
        ;(stroke 192 192 255 192))
        
      (doseq [endpoints endpoint-pairs]
        (draw-curve (endpoints 0) (endpoints 1))))))

; _______________________________________________________________________


(defn draw-normalized-facecodes [frame]
  (doseq [i (range (count @normalised-facecodes-sorted))]
    (let [x (* 5 (+ -15 (mod i 30)))
          y (* 5 (+ -10 (/ i 20)))
          z 0
          code (nth @normalised-facecodes-sorted i)]
      
      (push-matrix)
      (translate [x y z])
      (rotate-y (* frame 0.051471))
      ;(rotate-x (* frame 0.041471))
      ;(rotate-y (+ (* x y) (* frame 0.051471)))
      ;(rotate-x (+ (* x y) (* frame 0.041471)))

      (stroke-weight 2)
      (stroke 255 255 255 192)
      (no-fill)
      (draw-facecode code)
      
      (stroke-weight 1)
      (stroke 128 128 128 64)
      (no-fill)
      (draw-faces rd-verts rd-faces nil)
      (pop-matrix))))

; _______________________________________________________________________


(defn draw-tiling []
  ;(no-stroke)

  (doseq [tile (keys @tiles)]
    (let [pos tile]
      (with-translation pos 
        (push-matrix)
        (scale 0.5)
        ;(stroke-weight 2)
        ;(stroke 0 0 0  192)
        (stroke-weight 4)
        (stroke 192 192 255 192)
        (no-fill) 
        (draw-facecode (@tiles pos))
        
        (stroke-weight 1)
        (stroke 128 128 128 32)
        ;(no-stroke)
        (draw-faces rd-verts rd-faces nil)
        (stroke 0 255 0 32)
        ;(draw-todo)
        (pop-matrix)))))

