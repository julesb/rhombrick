(ns rhombrick.tiling-render
  (:use [quil.core]
        [rhombrick.vector]
        [rhombrick.staticgeometry]
        [rhombrick.facecode]
        [rhombrick.tiling]
        [rhombrick.glider]
        [clojure.math.combinatorics]))


(def rhomb-tex (atom nil))
(def current-tileset-colors (atom {}))

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



; this assumes that the tilecode can be parsed as a binary number - no use
;(defn compute-tile-color-orig [code]
;  (if (not= nil code)
;    (let [rc (set (rotations code))
;          bvals (map #(. Integer parseInt % 2) rc)
;          n (first (sort bvals))
;          c (int (mod n 12))]
;      (rd-face-colors c))
;    [255 0 0]))

(defn compute-tile-color-1 [code]
  (if (not= nil code)
    (let [n (hash (apply str (set (rotations code))))
          c (int (mod n 12))]
      (rd-face-colors c))
    [255 0 0]))

(defn compute-tile-color [code]
  (when (not= nil code)
    (let [hs (apply str "0x" code)
          n (mod (read-string hs) 12)]
      (rd-face-colors n))))


(defn get-tile-color [code]
  (if (contains? @current-tileset-colors code)
    (@current-tileset-colors code)
    [255 255 255 192]))


(defn get-group-color [code]
  (rd-face-colors (mod (count (get-connected-idxs code))
                       12)))


(defn get-unique-color [code])


(defn draw-connected-faces [code]
  (let [col (get-tile-color code)]
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
        
        (if (seq colors)
          (fill (/ (col 0) 3.0) (/ (col 1) 3.0) (/ (col 2) 3.0) 255 ))
        (begin-shape :quads)
        (vertex (v0 0) (v0 1) (v0 2))
        (vertex (v1 0) (v1 1) (v1 2))
        (vertex (v2 0) (v2 1) (v2 2))
        (vertex (v3 0) (v3 1) (v3 2))
        (end-shape))))


(defn draw-faces-lite [verts faces col]
  (apply stroke col)
  (doseq [i (range (count faces))]
    (let [vert-idx (faces i)
          v0 (verts (vert-idx 0))
          v1 (verts (vert-idx 1))
         v2 (verts (vert-idx 2))
          v3 (verts (vert-idx 3))]
      (begin-shape :quads)
      (vertex (v0 0) (v0 1) (v0 2))
      (vertex (v1 0) (v1 1) (v1 2))
      (vertex (v2 0) (v2 1) (v2 2))
      (vertex (v3 0) (v3 1) (v3 2))
      (end-shape))))

; _______________________________________________________________________


(defn draw-face-list []
  (fill 64 64 128 150)
  ;(no-fill)
  (stroke 128 128 255 64)
  (stroke-weight 2)
  ;(no-stroke)
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
  ;(tint 192 192 255 255)
  ;(stroke 0 0 0 192)
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
        ;num-connected (count (filter #(= \1 %) code))
        col (get-tile-color code)]
    (no-fill)
    ;(fill (col 0) (col 1) (col 2) 64)
    (stroke (col 0) (col 1) (col 2) 192)
    (stroke-weight 1)
    (with-translation pos
      (scale 0.5)
      (draw-faces rd-verts rd-faces nil))))
   

(defn draw-neighbours [pos]
  (no-fill)
  (stroke 64 64 64 64)
  (stroke-weight 1)

  (let [ipos (vec (map int pos))]
    (doseq [n (get-neighbours ipos)]
      (with-translation n
        (scale 0.5)
        (draw-faces rd-verts rd-faces nil)
        ))))

; _______________________________________________________________________


(defn draw-curve-boundary-points [pos]
  (when (contains? @tiles pos)
    (let [code (@tiles pos)
          num-connected (get-num-connected code)
          col (get-tile-color code)] ;(rd-face-colors (mod num-connected 12))]
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
                             )))))))
  

(defn draw-opposite-compatible-boundary-points [pos]
  (when (contains? @tiles pos)
    (let [code (@tiles pos)]
      (with-translation pos
        (scale 0.5)
        (stroke-weight 4)
        (doseq [i (range 12)]
          (let [d (.charAt code i)]
            (when (not (face-digit-like-compatible? d))
              (let [polarity (re-find #"[a-f]+" (str d))]
                (if polarity
                  (do (stroke 0 0 0 255)
                      (fill 255 255 255 255))
                  (do (stroke 255 255 255 255)
                      (fill 0 0 0 255)))
                (with-translation (co-verts i)
                  (when polarity
                    (rotate-x (/ Math/PI 4.0))
                    (rotate-y (/ Math/PI 4.0))
                    (rotate-z (/ Math/PI 4.0)))
                  (box 0.1))))))))))


(defn draw-empty []
  (fill 0 255 0 192)
  (doseq [tile @empty-positions]
    (let [pos tile]
      (with-translation pos 
        (scale 0.05)
        (box 1 1 1)
        ;(draw-faces rd-verts rd-faces nil)
        ;(draw-faces rd-verts rd-faces rd-face-colors)
        ))))
; _______________________________________________________________________


; given two face indices, returns a vec of four 3d bezier control points
(defn get-bezier-controls [f1-idx f2-idx]
  (let [p1 (co-verts f1-idx)
        p2 (vec3-scale p1 0.5)
        p4 (co-verts f2-idx)
        p3 (vec3-scale p4 0.5)]
    [p1 p2 p3 p4]))


(defn get-bezier-point-3d [f1-idx f2-idx t]
  (when (not= f1-idx f2-idx)
    (let [[p1 p2 p3 p4] (get-bezier-controls f1-idx f2-idx)
          bx (bezier-point (p1 0) (p2 0) (p3 0) (p4 0) t)
          by (bezier-point (p1 1) (p2 1) (p3 1) (p4 1) t)
          bz (bezier-point (p1 2) (p2 2) (p3 2) (p4 2) t)]
      [bx by bz])))


(defn get-bezier-tangent-3d [f1-idx f2-idx t]
  (when (not= f1-idx f2-idx)
    (let [[p1 p2 p3 p4] (get-bezier-controls f1-idx f2-idx)
          tx (bezier-tangent (p1 0) (p2 0) (p3 0) (p4 0) t)
          ty (bezier-tangent (p1 1) (p2 1) (p3 1) (p4 1) t)
          tz (bezier-tangent (p1 2) (p2 2) (p3 2) (p4 2) t)]
      [tx ty tz])))


(defn draw-curve-tangents [f1-idx f2-idx steps]
  (when (not= f1-idx f2-idx)
    (let [step (/ 1.0 steps)]
      (doseq [s (range steps)]
        (let [t (* s step)
              b (get-bezier-point-3d f1-idx f2-idx t)
              bt (get-bezier-tangent-3d f1-idx f2-idx t)
              bts (vec3-add b (vec3-scale bt 0.25))]
          (line (b 0) (b 1) (b 2) (bts 0) (bts 1) (bts 2)))))))
 

(defn draw-curve [f1-idx f2-idx]
  (when (not= f1-idx f2-idx)
    (let [[p1 p2 p3 p4] (get-bezier-controls f1-idx f2-idx)]
      (bezier (p1 0) (p1 1) (p1 2)
              (p2 0) (p2 1) (p2 2)
              (p3 0) (p3 1) (p3 2)
              (p4 0) (p4 1) (p4 2)))))


(defn make-curve-endpoints-orig [connected-idxs]
  (let [num-points (count connected-idxs)]
    (map #(vector %1 (nth connected-idxs (mod (+ %2 1) num-points)))
         connected-idxs (range num-points))))


(defn make-curve-endpoints [connected-idxs]
  (map vec (vec (combinations connected-idxs 2))))


(defn draw-gliders [frame]
  (do
    ;(fill 255 0 0 128)
    ;(sphere 0.25)
    (push-style)
    (stroke-weight 4)
    (stroke 255 255 192 192)
    ;(let [tile-color (get-group-color (@tiles ((get-glider 1) :current-tile)))]
    (doseq [glider @gliders]
      (let [pos (get-glider-pos (glider :id))
            col (glider :color)
            tile (glider :current-tile)]
        (if (contains? @tiles tile)
          (with-translation pos
            (fill (col 0) (col 1) (col 2) 128)
            ;(apply fill (glider :color))
            ;(scale 0.5)
            ;(sphere 0.2)
            ;(if (= (glider :id) 1) 
            ;   (point-light (tile-color 0) (tile-color 1) (tile-color 2)
            ;                ;0 0 0))
            ;                (pos 0) (pos 1) (pos 2)))

              ;(point-light 255 255 255 ;(col 0) (col 1) (col 2)
              ;           (pos 0) (pos 1) (- (pos 2) 0)))
              ;(push-matrix)
              (scale 0.01)
              (rotate-x (* frame (+ (glider :id) 20) 0.00351471))
              (rotate-y (* frame (+ (glider :id) 20) 0.00352363))
              ;(rotate-z (* frame (glider :id) 0.0035123))
              (draw-faces rd-verts rd-faces nil)
              ;(box 0.01 0.01 0.01)
              ;(pop-matrix)
                            ))))
    (pop-style)
    ))
     

(defn draw-facecode [code]
  (let [endpoint-pairs (make-curve-endpoints (get-connected-idxs code))
        num-connected (get-num-connected code)
        col (get-tile-color code); (rd-face-colors (mod num-connected 12))
        col2 (get-group-color code)
        fill-col (rd-face-colors 
                   (connecting-faces (mod num-connected 12)))
        ;col-idx (mod (Integer/parseInt code 2) 12)
        ]
    (push-style)

    (if (= code nil)
      (do 
        (fill 255 32 32 128)
        ;(no-fill)
        (stroke 255 0 0 192)
        (push-matrix)
        (scale 0.05)
        (box 1 1 1)
        ;(draw-faces rd-verts rd-faces nil)
        (pop-matrix)
        (no-fill)))

    (stroke-weight 2) 
    (stroke (col 0) (col 1) (col 2) 128) 
    ;(stroke 192 192 255 192)
    ;(fill (fill-col 0) (fill-col 1) (fill-col 2) 255)
    ;(stroke 150 150 255 128)
    
    (if (= num-connected 1)
      (let [p (co-verts (first (get-connected-idxs code)))]
        (line 0 0 0 (p 0) (p 1) (p 2))
        
        (fill 255 128 128 128)
        (box 0.05125 0.05125 0.05125)
        (no-fill))
        )

    (stroke-weight 4)
    (stroke (col 0) (col 1) (col 2) 192)
    ;(stroke 192 192 255 192)
    (doseq [endpoints endpoint-pairs]
      (draw-curve (endpoints 0) (endpoints 1)))
    
    ;draw tangent vectors
    (stroke-weight 1)
    (stroke 255 120 120 128)
    (doseq [endpoints endpoint-pairs]
      (draw-curve-tangents (endpoints 0) (endpoints 1) 3))

    ;(stroke-weight 2)
    ;(stroke (col2 0) (col2 1) (col2 2) 255)
    ;(doseq [endpoints endpoint-pairs]
    ;  (draw-curve (endpoints 0) (endpoints 1)))
    (pop-style)
      ))


(defn draw-facecode-lite [code]
  (let [endpoint-pairs (make-curve-endpoints (get-connected-idxs code))
        num-connected (count (filter #(= \1 %) code))
        ;col (get-tile-color code) ; (rd-face-colors (mod num-connected 12))
        col (get-group-color code) ;(rd-face-colors (mod num-connected 12))
        col (compute-tile-color code)
        ;col2 (get-group-color code)
        ;fill-col (rd-face-colors 
        ;           (connecting-faces (mod num-connected 12)))
        ;col-idx (mod (Integer/parseInt code 2) 12)
        ]
    (push-style)
    (if (= code nil)
      (do 
        (fill 255 32 32 128)
        ;(no-fill)
        (stroke 255 0 0 192)
        (push-matrix)
        (scale 0.05)
        (box 1 1 1)
        (pop-matrix)
        (no-fill)))
     
    (stroke (col 0) (col 1) (col 2) 128)
    ;(fill (fill-col 0) (fill-col 1) (fill-col 2) 255)
    ;(stroke 150 150 255 128)
    
    (if (= num-connected 1)
      (let [p (co-verts (first (get-connected-idxs code)))]
        (line 0 0 0 (p 0) (p 1) (p 2))
        (fill 255 128 128 128)
        (box 0.05125 0.05125 0.05125)
        (no-fill))
        )

    (stroke-weight 4)
    (stroke (col 0) (col 1) (col 2) 192)
    (doseq [endpoints endpoint-pairs]
      (draw-curve (endpoints 0) (endpoints 1)))
    
;    (stroke-weight 2)
;    (stroke (col 0) (col 1) (col 2) 192)
;    (doseq [endpoints endpoint-pairs]
;      (draw-curve (endpoints 0) (endpoints 1)))
    (pop-style)
      ))

; _______________________________________________________________________


(defn draw-tiling []
  (doseq [tile (keys @tiles)]
    (let [pos tile
          code (@tiles pos)]
      (draw-opposite-compatible-boundary-points pos) 

      (with-translation pos 
        (scale 0.5)
        (stroke-weight 8)
        (stroke 0 0 0 64)
        (no-fill) 
        (draw-facecode (@tiles pos))


        ;(stroke-weight 1)
        ;(stroke 128 128 128 24)
        ;;(no-stroke)
        ;(draw-faces rd-verts rd-faces nil)
        
        ;(stroke 0 255 0 32)
        ;(draw-todo)
      ))))

