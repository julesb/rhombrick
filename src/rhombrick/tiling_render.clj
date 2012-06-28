(ns rhombrick.tiling-render
  (:use [quil.core]
        [rhombrick.vector]
        [rhombrick.staticgeometry]
        [rhombrick.facecode]
        [rhombrick.tiling]
        [rhombrick.glider]))


(def rhomb-tex (atom nil))

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

(defn get-group-color [code]
  (rd-face-colors (mod (count (get-connected-idxs code))
                       12)))

(defn draw-connected-faces [code]
  (let [col (get-group-color code)]
    (fill (col 0) (col 1) (col 2) 128)
    (doseq [idx (get-connected-idxs code)]
      (let [vert-idx (rd-faces idx)
            v0 (rd-verts (vert-idx 0))
            v1 (rd-verts (vert-idx 1))
            v2 (rd-verts (vert-idx 2))
            v3 (rd-verts (vert-idx 3))]
        (begin-shape :quads)
        (vertex (v0 0) (v0 1) (v0 2))
        (vertex (v1 0) (v1 1) (v1 2))
        (vertex (v2 0) (v2 1) (v2 2))
        (vertex (v3 0) (v3 1) (v3 2))
        (end-shape)))))

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
          (fill (/ (col 0) 3.0) (/ (col 1) 3.0) (/ (col 2) 3.0) 255 ))
        (begin-shape :quads)
        (vertex (v0 0) (v0 1) (v0 2))
        (vertex (v1 0) (v1 1) (v1 2))
        (vertex (v2 0) (v2 1) (v2 2))
        (vertex (v3 0) (v3 1) (v3 2))
        (end-shape))))

; _______________________________________________________________________

(defn draw-face-list []
  (fill 64 64 128 255)
  ;(stroke 128 128 255 192)
  ;(stroke-weight 2)
  (no-stroke)
  (doseq [face-verts @face-list]
    (let [v0 (face-verts 0)
          v1 (face-verts 1)
          v2 (face-verts 2)
          v3 (face-verts 3)]
       
      (begin-shape :quads)
      ;(texture @rhomb-tex)
      (vertex (v0 0) (v0 1) (v0 2))
      (vertex (v1 0) (v1 1) (v1 2))
      (vertex (v2 0) (v2 1) (v2 2))
      (vertex (v3 0) (v3 1) (v3 2))
      (end-shape)
      ;(line (v0 0) (v0 1) (v0 2) (v2 0) (v2 1) (v2 2))
      )))


(defn draw-face-list-textured []
  (fill 255 255 255 255)
  ;(stroke 128 128 255 192)
  ;(stroke-weight 2)
  (no-stroke)
  (doseq [face-verts @face-list]
    (let [v0 (face-verts 0)
          v1 (face-verts 1)
          v2 (face-verts 2)
          v3 (face-verts 3)
          tex-coord-inset (/ 1.0 7.0)]
       
      (begin-shape :quads)
      (texture @rhomb-tex)
      (vertex (v0 0) (v0 1) (v0 2) tex-coord-inset 0.5)
      (vertex (v1 0) (v1 1) (v1 2) 0.5 0.0)
      (vertex (v2 0) (v2 1) (v2 2) (- 1.0 tex-coord-inset) 0.5)
      (vertex (v3 0) (v3 1) (v3 2) 0.5 1.0)

      (end-shape)
      ;(line (v0 0) (v0 1) (v0 2) (v2 0) (v2 1) (v2 2))
      )))

; _______________________________________________________________________

(defn draw-selected-tile [pos]
  (let [code (@tiles pos)
        num-connected (count (filter #(= \1 %) code))
        col (rd-face-colors (mod num-connected 12))]
    (no-fill)
    ;(fill (col 0) (col 1) (col 2) 64)
    (stroke (col 0) (col 1) (col 2) 192)
    (stroke-weight 4)
    (with-translation pos
      (scale 0.5)
      (draw-faces rd-verts rd-faces nil))))
   


(defn draw-neighbours [pos]
  (no-fill)
  (stroke 0 0 0 128)
  (stroke-weight 1)

  (let [ipos (vec (map int pos))]
  (doseq [n (get-neighbours ipos)]
    (with-translation n
    (scale 0.5)
    (draw-faces rd-verts rd-faces nil)
    ))))

; _______________________________________________________________________


(defn draw-curve-boundary-points [pos]
  ;(fill 32 32 255 128)
  (let [code (@tiles pos)
        num-connected (count (filter #(= \1 %) code))
        col (rd-face-colors (mod num-connected 12))]
    (no-stroke)
    (fill (col 0) (col 1) (col 2) 240)
    (with-translation pos
      (scale 0.5)
      (doseq [i (range 12)]
        (if (not= (.charAt code i) \0)
         (with-translation (co-verts i)
          (scale 0.02)
          ;(sphere 0.05)
          (draw-faces rd-verts rd-faces nil)
                           ))))))
                                    
; _______________________________________________________________________


(defn draw-todo []
  (doseq [tile  @todo]
    (let [pos tile]
      (with-translation pos 
        (push-matrix)
        (scale 0.5)
        ;(box 1 1 1)
        (draw-faces rd-verts rd-faces nil)
        ;(draw-faces rd-verts rd-faces rd-face-colors)
        (pop-matrix)))))

; _______________________________________________________________________


(defn draw-todo-head []
  ;(fill 255 255 0 255)
  (stroke 128 128 255 220)
  (stroke-weight 4)
  (if (not (empty? @todo)) 
    (with-translation (peek @todo)
      (push-matrix)
      (scale 0.55)
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



(defn make-curve-endpoints [connected-idxs]
  (let [num-points (count connected-idxs)]
    (map #(vector %1 (nth connected-idxs (mod (+ %2 1) num-points)))
         connected-idxs (range num-points))))

; _______________________________________________________________________


(defn draw-gliders [frame]
  (do
    ;(fill 255 0 0 128)
    ;(sphere 0.25)
    (stroke-weight 4)
    (let [tile-color (get-group-color (@tiles ((get-glider 1) :current-tile)))]
    (doseq [glider @gliders]
      (let [pos (get-glider-pos (glider :id))
            col (glider :color)]
        ;(sphere 10)))))
        (with-translation pos
          (fill (col 0) (col 1) (col 2) 128)
          ;(apply fill (glider :color))
          ;(scale 0.5)
          ;(sphere 0.2)
          ;(if (= (glider :id) 1) 
          ;   (point-light (tile-color 0) (tile-color 1) (tile-color 2)
          ;                0 0 0))
                          ;(pos 0) (pos 1) (pos 2)))

            ;(point-light 255 255 255 ;(col 0) (col 1) (col 2)
            ;           (pos 0) (pos 1) (- (pos 2) 0)))
            (push-matrix)
            (rotate-x (* frame (+ (glider :id) 20) 0.00351471))
            (rotate-y (* frame (+ (glider :id) 20) 0.00352363))
            ;(rotate-z (* frame (glider :id) 0.0035123))
            (box 0.01 0.01 0.01)
            (pop-matrix)
                          ))))))
     

; _______________________________________________________________________


(defn draw-facecode [code]
  (let [endpoint-pairs (make-curve-endpoints (get-connected-idxs code))
        num-connected (count (filter #(= \1 %) code))
        col (rd-face-colors (mod num-connected 12))
        fill-col (rd-face-colors 
                   (connecting-faces (mod num-connected 12)))
        ;col-idx (mod (Integer/parseInt code 2) 12)
        ]

    (if (= code "xxxxxxxxxxxx")
      (do 
        ;(fill 32 32 96 255)
        (no-fill)
        (stroke 0 0 0 192)
        (push-matrix)
        (scale 0.95)
        (draw-faces rd-verts rd-faces nil)
        (pop-matrix)
        (no-fill)))
     
    (stroke (col 0) (col 1) (col 2) 92) 
    ;(fill (fill-col 0) (fill-col 1) (fill-col 2) 255)
    ;(stroke 150 150 255 128)
    
    (if (= num-connected 1)
      (let [p (co-verts (first (get-connected-idxs code)))]
        (line 0 0 0 (p 0) (p 1) (p 2))
        
        ;(no-stroke)
        (fill 255 128 128 128)
        (box 0.05125 0.05125 0.05125)
        ;(sphere 0.125)
        (no-fill))
        )
    ;(if (> num-connected 2)
    ;  (fill 255 128 64 128))
    
    ;(if (> num-connected 2)
    ;  (begin-shape))

    (doseq [endpoints endpoint-pairs]
      ;(push-matrix)
      ;(scale 0.01)
      ;(box 1 1 1)
      ;(pop-matrix)
      (draw-curve (endpoints 0) (endpoints 1)))
    
    ;(if (> num-connected 2)
    ;  (end-shape))

      ))
    ;))
; _______________________________________________________________________


(defn draw-normalized-facecodes [frame]
  (fill 0 0 0 240)
  (with-translation [0 0 -5]
    (box 200 130 1))
  
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
        ;(push-matrix)
        (scale 0.5)
        ;(stroke-weight 2)
        ;(stroke 0 0 0  192)
        (stroke-weight 16)
        ;(stroke 150 150 255 64)
        (stroke 0 0 0 64)
        (no-fill) 
        (draw-facecode (@tiles pos))
        
        (stroke-weight 1)
        (stroke 128 128 128 128)
        ;;(no-stroke)
        ;(draw-faces rd-verts rd-faces nil)
        
        (stroke 0 255 0 32)
        ;(draw-todo)
        ;(pop-matrix)
      ))))

