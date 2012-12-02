(ns rhombrick.tiling-render
  (:use [quil.core]
        [rhombrick.vector]
        [rhombrick.staticgeometry]
        [rhombrick.facecode]
        [rhombrick.tiling]
        [rhombrick.camera]
        [rhombrick.glider]
        [clojure.math.combinatorics]))


(def rhomb-tex (atom nil))
(def current-tileset-colors (atom {}))
(def model-scale (atom 50))
(def bezier-box-resolution (atom 8))
(def face-list (atom #{}))
(def face-id-text (atom []))

; map facecode digit to bezier anchor scale
(def bezier-box-thicknesses {\0 0.125
                             \1 0.25
                             \2 0.5
                             \3 0.75
                             \4 1.0
                             \5 1.4142135623730951
                             \6 2.0
                             \a 0.125
                             \A 0.125
                             \b 0.25
                             \B 0.25
                             \c 0.5
                             \C 0.5
                             \d 0.75
                             \D 0.75
                             \e 1.0
                             \E 1.0
                             \f 1.4142135623730951
                             \F 1.4142135623730951
                             })


;(defn draw-rhomb-verts []
;  (stroke 0 0 255 255)
;  (doseq [v rd-verts]
;    (point (v 0) (v 1) (v 2))
;    ))
; _______________________________________________________________________
;
;
;(defn draw-verts [vert-array]
;  (doseq [v vert-array]
;    (point (v 0) (v 1) (v 2))
;    ))
;
; _______________________________________________________________________




;(defn compute-tile-color-1 [code]
;  (if (not= nil code)
;    (let [n (hash (apply str (set (rotations code))))
;          c (int (mod n 12))]
;      (rd-face-colors c))
;    [255 0 0]))


(defn draw-graph [[x y] w h title data-range data]
  (stroke-weight 1)
  (stroke 128 128 128 255)
  (fill 0 0 0 192)
  (rect x y w h)
  (stroke 192 192 255 255)
  (no-fill)
  (doseq [i (range (count data))]
    (if-let [d (data i)]
      (when (> d 0)
        (let [x1 (+ x i)
              y1 (+ y h)
              x2 (+ x i)
              y2 (- y1 (* (/ d data-range) h))]
          (point x2 y2))))))


(defn get-graph-params []
  [{:title "tiles"
    :range @max-tiles
    :data @stats-tile-count}
   {:title "iter time"
    :range 500
    :data @stats-iter-time}
   {:title "backtrack"
    :range (max 100 (count @tiles))
    :data @stats-backtrack}
  ])


(defn draw-graphs [[x y]]
  (let [graph-height 50
        graph-space 5
        graphs (get-graph-params)]
    (doseq [i (range (count graphs))]
      (let [graph (graphs i)
            gx x
            gy (+ y (* i graph-height) (* i graph-space))]
        (draw-graph [gx gy]
                    stats-buffer-length
                    graph-height
                    (graph :title)
                    (graph :range)
                    (graph :data))))))



(defn facecode-to-hex [code]
  (apply str "0x" (map #(if (= \- %) \0 %) code)))


(defn compute-tile-color [code]
  (if (not= nil code)
    (let [hs (facecode-to-hex code)
          n (mod (read-string hs) 12)]
      (rd-face-colors n))
    [128 128 128 128]))


(defn get-tile-color [code]
  (if (contains? @current-tileset-colors code)
    (@current-tileset-colors code)
    (do 
      (swap! current-tileset-colors assoc code (compute-tile-color code))
      (@current-tileset-colors code))))


;(defn get-group-color [code]
;  (rd-face-colors (mod (count (get-connected-idxs code))
;                       12)))


;(defn draw-connected-faces [code]
;  (let [col (get-tile-color code)]
;    (fill (col 0) (col 1) (col 2) 128)
;    (doseq [idx (get-connected-idxs code)]
;      (let [vert-idx (rd-faces idx)
;            v0 (rd-verts (vert-idx 0))
;            v1 (rd-verts (vert-idx 1))
;            v2 (rd-verts (vert-idx 2))
;            v3 (rd-verts (vert-idx 3))]
;        (begin-shape :quads)
;        (vertex (v0 0) (v0 1) (v0 2))
;        (vertex (v1 0) (v1 1) (v1 2))
;        (vertex (v2 0) (v2 1) (v2 2))
;        (vertex (v3 0) (v3 1) (v3 2))
;        (end-shape)))))
;

(defn draw-faces [verts faces colors]
  (doseq [i (range (count faces))]
      (let [vert-idx (faces i)
            v0 (verts (vert-idx 0))
            v1 (verts (vert-idx 1))
            v2 (verts (vert-idx 2))
            v3 (verts (vert-idx 3))
            col-idx (mod i (count colors))
            [r g b a] (if (= (count colors) 12) (colors col-idx) colors)]
        
        (if (seq colors)
          (stroke r g b a))
        ;  (fill (/ (col 0) 3.0) (/ (col 1) 3.0) (/ (col 2) 3.0) 255 ))
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


;(defn draw-faces-with-info [verts faces col]
;  (reset! face-id-text [])
;  (apply stroke col)
;  (doseq [i (range (count faces))]
;    (let [vert-idx (faces i)
;          v0 (verts (vert-idx 0))
;          v1 (verts (vert-idx 1))
;          v2 (verts (vert-idx 2))
;          v3 (verts (vert-idx 3))
;          sx (screen-x (v0 0) (v0 1) (v0 2))
;          sy (screen-y (v0 0) (v0 1) (v0 2))
;          sz (screen-z (v0 0) (v0 1) (v0 2))
;          v0-sc (vec3-scale v0 50)]
;      ;(println "screen-z:" sz)
;      (no-fill)
;      (begin-shape :quads)
;      (vertex (v0 0) (v0 1) (v0 2))
;      (vertex (v1 0) (v1 1) (v1 2))
;      (vertex (v2 0) (v2 1) (v2 2))
;      (vertex (v3 0) (v3 1) (v3 2))
;      (end-shape)
;      (swap! face-id-text conj [i v0-sc])
;      ;(hint :disable-depth-test)
;      ;(fill 255 255 255 255)
;      ;(text (str i) sx sy)
;      ;(hint :enable-depth-test)
;      )))
; _______________________________________________________________________

(defn face-idxs-to-verts [face-idxs]
  (vec (map #(rd-verts %) face-idxs)))


(defn facelist-contains-rotations? [face-verts]
  (or
    (> (count (filter #(contains? @face-list %)
                      (rotations-vec face-verts)))
       0)
    (> (count (filter #(contains? @face-list %)
                      (rotations-vec (vec (reverse face-verts)))))
       0)   
       ))


(defn remove-from-facelist [face-verts]
    (let [face-rots (concat (rotations-vec face-verts) 
                            (rotations-vec (reverse face-verts)))]
        (doseq [f face-rots]
          (if (contains? @face-list f)
            (swap! face-list disj f)))))
      
  

; we dont need to check every face in the face list here
; only need to check the neighbours.
(defn add-tile-to-facelist [pos]
  (doseq [f rd-faces]
    (let [fv (face-idxs-to-verts f)
          fvw (vec (map #(vec3-add pos (vec3-scale % 0.5)) fv))]
    (if (not (facelist-contains-rotations? fvw))
      (swap! face-list conj fvw)
      (do
        (remove-from-facelist fvw))))))


(defn build-face-list []
  (reset! face-list #{})
  (doseq [tile (keys @tiles)]
    (add-tile-to-facelist tile)))


(defn draw-face-list []
  (fill 32 32 32 128)
  ;(no-fill)
  ;(stroke 0 0 0 192)
  (stroke 40 40 40 190)
  (stroke-weight 4)
  ;(no-stroke)
  (doseq [face-verts @face-list]
    (let [v0 (face-verts 0)
          v1 (face-verts 1)
          v2 (face-verts 2)
          v3 (face-verts 3)]
      (stroke 40 40 40 190) 
      (begin-shape :quads)
      ;(texture @rhomb-tex)
      (vertex (v0 0) (v0 1) (v0 2))
      (vertex (v1 0) (v1 1) (v1 2))
      (vertex (v2 0) (v2 1) (v2 2))
      (vertex (v3 0) (v3 1) (v3 2))
      (end-shape)
      (stroke 40 40 40 128)
      (line (v0 0) (v0 1) (v0 2) (v2 0) (v2 1) (v2 2))
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
    (stroke (col 0) (col 1) (col 2) 16)
    (stroke-weight 8)
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


;(defn draw-curve-boundary-points [pos]
;  (when (contains? @tiles pos)
;    (let [code (@tiles pos)
;          num-connected (get-num-connected code)
;          col (get-tile-color code)] ;(rd-face-colors (mod num-connected 12))]
;      (no-stroke)
;      (fill (col 0) (col 1) (col 2) 240)
;      (with-translation pos
;        (scale 0.5)
;        (doseq [i (range 12)]
;          (if (not= (.charAt code i) \-)
;           (with-translation (co-verts i)
;            (scale 0.02)
;            ;(sphere 0.05)
;            (draw-faces rd-verts rd-faces nil)
;                             )))))))
;  

(defn draw-face-boundaries-basic [pos]
  (when (contains? @tiles pos)
    (stroke-weight 4)
    (stroke 0 0 0 255)
    (fill 255 255 255 255)
    (with-translation pos
      (scale 0.5)
      (doseq [i (range 12)]
        (let [[dx dy dz] (co-verts i)
              [dxn dyn dzn] (vec3-normalize [dx dy dz])
              az (Math/atan2 dyn dxn)
              el (- (Math/asin dzn))]
          (with-translation (co-verts i)
            (rotate az 0 0 1)
            (rotate el 0 1 0)
            (box 0.2)))))))


(defn draw-face-boundaries [pos ^String code]
  (when (contains? @tiles pos)
    (let [[r g b] (get-tile-color code)]
      (with-translation pos
        (scale 0.5)
        ;(stroke-weight 1)
        (no-stroke)
        ;(stroke r g b 255)
        (doseq [^long i (range 12)]
          (when (and (not= (.charAt code i) \-))
                     ;(is-empty? @tiles (get-neighbour-pos pos i)))
            (let [d (.charAt code i)
                  dir (co-verts i)
                  [dx dy dz] (vec3-normalize dir)
                  az (Math/atan2 dy dx)
                  el (- (Math/asin dz))
                  thickness (* 1.3 (bezier-box-thicknesses (.charAt code i)))]
              (if (face-digit-like-compatible? d)
                (do (fill 160 160 220 255))
                (do
                  (if (>= (int d) 97)
                    (fill 255 255 255 255)
                    (fill 0 0 0 255))))
              (with-translation (vec3-scale (co-verts i) 0.91)
                (rotate az 0 0 1)
                (rotate el 0 1 0)
                ;(rotate (/ Math/PI 4.0) 1 0 0)
                (box 0.25 thickness thickness)))))))))


(defn draw-color-markers [pos]
  (no-lights)
  (doseq [i (range 12)]
    (let [[r g b] (rd-face-colors i)]
      (with-translation pos
        (scale 0.5)
        (stroke-weight 1)
        (stroke 255 255 255 128)
        ;(stroke (- 255 r) (- 255 g) (- 255 b) 255)
        (fill r g b 255)
        (let [dir (co-verts i)
             [dx dy dz] (vec3-normalize dir)
             az (Math/atan2 dy dx)
             el (- (Math/asin dz))]
          (with-translation (vec3-scale (co-verts i) 0.975)
            (rotate az 0 0 1)
            (rotate el 0 1 0)
            (box 0.05 0.5 0.5)
                            ))))))


(defn draw-face-idx-numbers [pos use-face-color?]
  (no-lights)
  (doseq [i (range 12)]
    (let [[r g b] (rd-face-colors i)]
      (with-translation pos
        (scale 0.5)
        (stroke-weight 1)
        (stroke 255 255 255 128)
        ;(stroke (- 255 r) (- 255 g) (- 255 b) 255)
        ;(fill r g b 255)
        (fill 255 255 255 255)                
        (let [dir (co-verts i)
             [dx dy dz] (vec3-normalize dir)
             az (Math/atan2 dy dx)
             el (- (Math/asin dz))
             tw (text-width (str i))]
          (if use-face-color?
            (fill r g b 192)
            (fill 255 255 255 192))
          (with-translation (vec3-scale (co-verts i) 1.01) ;0.975)
            (rotate az 0 0 1)
            (rotate el 0 1 0)
            (scale 0.025)
            (rotate-y (* Math/PI 0.5))
            (if use-face-color?
              (translate -10 0 0)
              (translate 10 0 0))
            (text (str i) (- tw) 0 0)                
                            ))))))


(defn get-bezier-anchor-offsets-rotated [f-idx]
  (let [o (bezier-anchor-offsets f-idx)]
    (vec [(vec3-bisect (o 0) (o 1)) 
              (vec3-bisect (o 1) (o 2))
              (vec3-bisect (o 2) (o 3))
              (vec3-bisect (o 3) (o 0))])))


; this is hideous and horrible but as long as it spits out the right numbers
; it will do, since it will only be used for precomputing triangle strip data 
(defn get-bezier-anchor-offsets [f1-idx f2-idx]
  (let [f1-center (co-verts f1-idx)
        f2-center (co-verts f2-idx)
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
        f2-offsets-swapped (if (= 120 (round (get-angle-for-face-idxs [f1-idx f2-idx])))
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


(defn draw-bezier-anchor-test [pos]
  (doseq [f-idx (range 12)]
    (when (= (count (bezier-anchor-offsets f-idx)) 4)
      (let [offsets [((get-bezier-anchor-offsets-rotated f-idx) 0)
                     ((get-bezier-anchor-offsets-rotated f-idx) 1)
                     ((get-bezier-anchor-offsets-rotated f-idx) 2)
                     ((get-bezier-anchor-offsets-rotated f-idx) 3)]
            colors [[255 0 0] [255 128 0] [255 255 0] [0 255 0]] ]
        ; draw anchors
        (stroke-weight 4)
        (doseq [i (range 4)] 
          (let [off-vec (vec3-scale (offsets i) 0.5)
                [x1 y1 z1] (co-verts f-idx)
                [x2 y2 z2] (vec3-add (co-verts f-idx) off-vec)]
            (apply stroke (colors i))
            (with-translation pos
              (scale 0.5)
              (line x1 y1 z1 x2 y2 z2))))
        ))))


(defn draw-empty []
  (fill 0 255 0 192)
  (doseq [tile (get-empty-positions)]
    (let [pos tile]
      (with-translation pos 
        (scale 0.05)
        (box 1 1 1)
        ;(draw-faces rd-verts rd-faces nil)
        ;(draw-faces rd-verts rd-faces rd-face-colors)
        ))))


(defn draw-assemblage-center []
  (let [[cx cy cz] @assemblage-center]
    (stroke 255 0 0 192)
    (stroke-weight 4)
    ;(fill 255 255 0 32)
    (no-fill)
    (with-translation (find-assemblage-center @tiles)
      (scale 0.5)
      (box 1 1 1)
      ;(draw-faces rd-verts rd-faces nil)
      )))

; _______________________________________________________________________


(defn get-bezier-controls [f1-idx f2-idx]
  "Given two face indices, returns a vec of four 3d bezier control points"
  (let [p1 (co-verts f1-idx)
        p2 (vec3-scale p1 0.5)
        p4 (co-verts f2-idx)
        p3 (vec3-scale p4 0.5)]
    [p1 p2 p3 p4]))


(defn get-bezier-controls-with-offset [f1-idx f2-idx f1-offset f2-offset]
  "Given two face indices, returns a vec of four 3d bezier control points"
  "offset is a vector to specify the offset for bezier boxes."
  (let [p1 (vec3-add (co-verts f1-idx) f1-offset)
        p2 (vec3-sub p1 (vec3-scale (co-verts f1-idx) 0.5))
        p4 (vec3-add (co-verts f2-idx) f2-offset)
        p3 (vec3-sub p4 (vec3-scale (co-verts f2-idx) 0.5))
        ]
    [p1 p2 p3 p4]))


(defn get-bezier-point-3d [f1-idx f2-idx t]
  (when (not= f1-idx f2-idx)
    (let [[p1 p2 p3 p4] (get-bezier-controls f1-idx f2-idx)
          bx (bezier-point (p1 0) (p2 0) (p3 0) (p4 0) t)
          by (bezier-point (p1 1) (p2 1) (p3 1) (p4 1) t)
          bz (bezier-point (p1 2) (p2 2) (p3 2) (p4 2) t)]
      [bx by bz])))


(defn get-bezier-point [[p1 p2 p3 p4] t]
  [ (bezier-point (p1 0) (p2 0) (p3 0) (p4 0) t)
    (bezier-point (p1 1) (p2 1) (p3 1) (p4 1) t)
    (bezier-point (p1 2) (p2 2) (p3 2) (p4 2) t)])


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


;(defn draw-curve-solid [f1-idx f2-idx steps]
;  (when (not= f1-idx f2-idx)
;    (doseq [i (range 1 (inc steps))]
;      (let [t (* i (/ 1 steps))
;            bp (get-bezier-point-3d f1-idx f2-idx t)
;            prev-bp (get-bezier-point-3d f1-idx f2-idx (- t (/ 1 steps)))
;            pos (vec3-scale (vec3-add bp prev-bp) 0.5)
;            [dx dy dz] (vec3-normalize (vec3-sub bp prev-bp))
;            az (Math/atan2 dy dx)
;            el (- (Math/asin dz))]
;        (with-translation pos ; bp
;          (scale 0.5)
;          (rotate az 0 0 1)
;          (rotate el 0 1 0)
;          (box 0.25 0.5 0.5))))))
;

(defn draw-curve [f1-idx f2-idx]
  (when (not= f1-idx f2-idx)
    (let [[p1 p2 p3 p4] (get-bezier-controls f1-idx f2-idx)]
      (bezier (p1 0) (p1 1) (p1 2)
              (p2 0) (p2 1) (p2 2)
              (p3 0) (p3 1) (p3 2)
              (p4 0) (p4 1) (p4 2)))))


(defn draw-curve-with-controls [[p1 p2 p3 p4]]
  (bezier (p1 0) (p1 1) (p1 2)
          (p2 0) (p2 1) (p2 2)
          (p3 0) (p3 1) (p3 2)
          (p4 0) (p4 1) (p4 2)))


(defn make-curve-endpoints-orig [connected-idxs]
  (let [num-points (count connected-idxs)]
    (map #(vector %1 (nth connected-idxs (mod (+ %2 1) num-points)))
         connected-idxs (range num-points))))


;(defn make-curve-endpoints [connected-idxs]
;  (if (> (count connected-idxs) 1)
;    (map vec (vec (combinations connected-idxs 2)))
;    [[(nth connected-idxs 0) (nth connected-idxs 0)]]))

(defn make-curve-endpoints [connected-idxs]
  (map vec (vec (combinations connected-idxs 2))))


(def bezier-box-tristrip-cache (atom {}))
(def bezier-box-line-cache (atom {}))

(defn bezier-box-tristrip-cache-reset []
  (reset! bezier-box-tristrip-cache {}))


(defn bezier-box-line-cache-reset []
  (reset! bezier-box-line-cache {}))


(defn bezier-box-cache-reset []
  (bezier-box-tristrip-cache-reset)
  (bezier-box-line-cache-reset))


(defn get-bezier-point-fake [[p1 p2 p3 p4] t]
  [0 0 0])


(defn get-bezier-strip [c1 c2 steps]
  (map #(let [t (* % (/ 1 steps))]
         [(get-bezier-point c1 t)
          (get-bezier-point c2 t)])
       (range (inc steps))))


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


(defn draw-bezier-box2 [f1-idx f2-idx f1-scale f2-scale steps]
  (let [strips (make-bezier-box-triangles f1-idx f2-idx f1-scale f2-scale steps)]
    (doseq [strip strips]
      (begin-shape :triangle-strip)
      (doseq [[vx vy vz] strip]
        (vertex vx vy vz))
      (end-shape))))


(defn make-facecode-bezier-box-triangles [^String code steps]
  (let [num-connected (get-num-connected code)
        endpoint-pairs (if (< num-connected 4)
                         (vec (make-curve-endpoints (get-connected-idxs code)))
                         (vec (filter #(not= 6 (abs (- (% 1) (% 0))))
                                      (make-curve-endpoints (get-connected-idxs code)))))]
    (map #(let [f1-thickness (bezier-box-thicknesses (.charAt code (% 0)))
                f2-thickness (bezier-box-thicknesses (.charAt code (% 1)))]
            (make-bezier-box-triangles (% 0) (% 1) f1-thickness f2-thickness steps))
         endpoint-pairs)))


(defn make-facecode-bezier-box-lines [^String code steps]
  (let [num-connected (get-num-connected code)
        endpoint-pairs (if (< num-connected 4)
                         (vec (make-curve-endpoints (get-connected-idxs code)))
                         (vec (filter #(not= 6 (abs (- (% 1) (% 0))))
                                      (make-curve-endpoints (get-connected-idxs code)))))]
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


(defn get-bezier-box-lines [code steps]
  "Gets bezier box outline verts from cache if present, otherwise computes "
  "the verts, adds them to the cache, then returns them."
  (when (not (contains? @bezier-box-line-cache code))
    (let [lines (make-facecode-bezier-box-lines code steps)]
      (swap! bezier-box-line-cache assoc code lines)))
  (@bezier-box-line-cache code))


(defn draw-bezier-box-slow [f1-idx f2-idx f1-scale f2-scale steps]
  (let [[f1-off f2-off] (get-bezier-anchor-offsets f1-idx f2-idx)
        f1-offsets (map #(vec3-scale % f1-scale) f1-off)
        f2-offsets (map #(vec3-scale % f2-scale) f2-off)
        controls (vec (map #(get-bezier-controls-with-offset f1-idx f2-idx %1 %2)
                       f1-offsets f2-offsets))]
    (doseq [c1-idx (range (count controls))]
      (let [c2-idx (mod (inc c1-idx) 4)
            c1 (controls c1-idx)
            c2 (controls c2-idx)]
        (begin-shape :triangle-strip)
        (doseq [i (range 0 (inc steps))]
          (let [t (* i (/ 1 steps))
                p1 (get-bezier-point c1 t)
                p2 (get-bezier-point c2 t)
                center (get-bezier-point-3d f1-idx f2-idx t)
                ;n1 (vec3-normalize (vec3-sub center p1))
                ;n2 (vec3-normalize (vec3-sub center p2))
                ]
            ;(apply normal n1)
            (apply vertex p1)
            ;(apply normal n2)
            (apply vertex p2)))
        (end-shape)
        ))))


(defn draw-bezier-box-lines [f1-idx f2-idx f1-scale f2-scale steps]
  (let [[f1-off f2-off] (get-bezier-anchor-offsets f1-idx f2-idx)
         f1-offsets (map #(vec3-scale % f1-scale) f1-off)
         f2-offsets (map #(vec3-scale % f2-scale) f2-off)
         controls (vec (map #(get-bezier-controls-with-offset f1-idx f2-idx %1 %2)
                            f1-offsets f2-offsets))]
    (doseq [c controls]
      (draw-curve-with-controls c))))
        


(defn draw-gliders [frame]
  (do
    ;(fill 255 0 0 128)
    ;(sphere 0.25)
    (push-style)
    (stroke-weight 4)
    (stroke 255 255 192 192)
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
        fill-col (rd-face-colors 
                   (connecting-faces (mod num-connected 12)))
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

    (stroke-weight 4) 
    (stroke (col 0) (col 1) (col 2) 192) 
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

    (stroke-weight 6)
    ;(fill (col2 0) (col2 1) (col2 2) 32)
    ;(stroke (col2 0) (col2 1) (col2 2) 32)

    (stroke (col 0) (col 1) (col 2) 192)
    ;(stroke 192 192 255 192)
    (doseq [endpoints endpoint-pairs]
      (draw-curve (endpoints 0) (endpoints 1)))
    
    ;draw tangent vectors
    ;(stroke-weight 1)
    ;(stroke 255 120 120 128)
    ;(doseq [endpoints endpoint-pairs]
    ;  (draw-curve-tangents (endpoints 0) (endpoints 1) 3))

    ;(stroke-weight 2)
    ;(stroke (col2 0) (col2 1) (col2 2) 255)
    ;(doseq [endpoints endpoint-pairs]
    ;  (draw-curve (endpoints 0) (endpoints 1)))
    (pop-style)
      ))


(defn draw-facecode-bezier-box-lines [code col steps]
  (apply stroke col)
  (stroke-weight 2)
  (no-fill)
  (doseq [line-verts (get-bezier-box-lines code steps)]
    (doseq [vert line-verts]
      (begin-shape)
      (doseq [[vx vy vz] vert]
        (vertex vx vy vz))
      (end-shape))))


(defn draw-facecode-bezier-boxes [code col steps]
  (when (contains? #{3 4} (count col)) (apply fill col))
  ;(stroke 0 0 0 192)
  (no-stroke)
  ;(stroke-weight 2)
  (doseq [bbox (get-bezier-box-triangles code steps)]
    (doseq [strip bbox]
      (begin-shape :triangle-strip)
      (doseq [[vx vy vz] strip]
        (vertex vx vy vz))
      (end-shape))))


(defn draw-facecode-color [^String code col]
  (let [num-connected (get-num-connected code)
        endpoint-pairs (if (< num-connected 4)
                         (vec (make-curve-endpoints (get-connected-idxs code)))
                         (vec (filter #(not= 6 (abs (- (% 1) (% 0))))
                                      (make-curve-endpoints (get-connected-idxs code)))))
        weight (- 9 (* (/ num-connected 12) 8))]
    (push-style)

    (if (= code nil)
      (do 
        (fill 255 32 32 128)
        (stroke 255 0 0 192)
        (push-matrix)
        (scale 0.05)
        (box 1 1 1)
        (pop-matrix)
        (no-fill)))

    (stroke-weight 2)

    (stroke (col 0) (col 1) (col 2) (col 3)) 
    
    (if (= num-connected 1)
      (let [p (co-verts (first (get-connected-idxs code)))]
        (line 0 0 0 (p 0) (p 1) (p 2))
        
        (fill 255 128 128 128)
        (box 0.05125 0.05125 0.05125)
        (no-fill))
        )

    ;(stroke-weight weight)
    ;(stroke (col 0) (col 1) (col 2) (col 3))
    ;(doseq [endpoints endpoint-pairs]
    ;  (draw-curve (endpoints 0) (endpoints 1)))
 
    (no-stroke)
    ;(stroke-weight 1)
    (fill (col 0) (col 1) (col 2) 255)
    ;(stroke (col 0) (col 1) (col 2) 255)
    ;(no-fill)
    (doseq [endpoints endpoint-pairs]
      (let [f1-thickness (bezier-box-thicknesses (.charAt code (endpoints 0)))
            f2-thickness (bezier-box-thicknesses (.charAt code (endpoints 1)))
            ]
        (no-stroke)
        ;(stroke 0 0 0 255)
        ;(fill (- 255 (col 0)) (- 255 (col 1)) (- 255 (col 2)) 255)
        ;(draw-curve-solid (endpoints 0) (endpoints 1) 8)

        (no-stroke)
        (fill (col 0) (col 1) (col 2) 255)
        (draw-bezier-box2 (endpoints 0) (endpoints 1)
                          f1-thickness f2-thickness
                          @bezier-box-resolution)
        ;(make-bezier-box-triangles 1 2 0.5 0.5 8)
        
        ;(stroke (col 0) (col 1) (col 2) 255)
        (stroke 128 128 128 255)
        (no-fill)
        (draw-bezier-box-lines (endpoints 0) (endpoints 1)
                               f1-thickness f2-thickness
                               @bezier-box-resolution)
        ))
    
    ;draw tangent vectors
    ;(stroke-weight 1)
    ;(stroke 255 120 120 128)
    ;(doseq [endpoints endpoint-pairs]
    ;  (draw-curve-tangents (endpoints 0) (endpoints 1) 3))

    (pop-style)
      ))


;(defn draw-facecode-lite [code]
;  (let [endpoint-pairs (make-curve-endpoints (get-connected-idxs code))
;        num-connected (count (filter #(= \1 %) code))
;        ;col (get-tile-color code) ; (rd-face-colors (mod num-connected 12))
;        col (get-group-color code) ;(rd-face-colors (mod num-connected 12))
;        col (compute-tile-color code)
;        ;col2 (get-group-color code)
;        ;fill-col (rd-face-colors 
;        ;           (connecting-faces (mod num-connected 12)))
;        ;col-idx (mod (Integer/parseInt code 2) 12)
;        ]
;    (push-style)
;    (if (= code nil)
;      (do 
;        (fill 255 32 32 128)
;        ;(no-fill)
;        (stroke 255 0 0 192)
;        (push-matrix)
;        (scale 0.05)
;        (box 1 1 1)
;        (pop-matrix)
;        (no-fill)))
;     
;    (stroke (col 0) (col 1) (col 2) 128)
;    ;(fill (fill-col 0) (fill-col 1) (fill-col 2) 255)
;    ;(stroke 150 150 255 128)
;    
;    (if (= num-connected 1)
;      (let [p (co-verts (first (get-connected-idxs code)))]
;        (line 0 0 0 (p 0) (p 1) (p 2))
;        (fill 255 128 128 128)
;        (box 0.05125 0.05125 0.05125)
;        (no-fill))
;        )
;
;    (stroke-weight 4)
;    (stroke (col 0) (col 1) (col 2) 192)
;    (doseq [endpoints endpoint-pairs]
;      (draw-curve (endpoints 0) (endpoints 1)))
;    
;;    (stroke-weight 2)
;;    (stroke (col 0) (col 1) (col 2) 192)
;;    (doseq [endpoints endpoint-pairs]
;;      (draw-curve (endpoints 0) (endpoints 1)))
;    (pop-style)
;      ))

; _______________________________________________________________________




(defn draw-tiling [with-boundaries? with-lines? with-bb-faces? with-bb-lines?]
  (doseq [tile (keys @tiles)]
    (let [pos tile
          code (@tiles pos)
          col (conj (get-tile-color code) 255)
          ;line-col [(col 0) (col 1) (col 2) 255]
          line-col [0 0 0 192]
          bezier-steps @bezier-box-resolution]
      (when with-boundaries?
        (draw-face-boundaries pos code))
      (with-translation pos 
        (scale 0.5)
        ;(stroke-weight 8)
        ;(stroke 0 0 0 64)
        (when with-lines?
          (no-fill)
          (draw-facecode code))
        (when with-bb-faces?
          (draw-facecode-bezier-boxes (@tiles pos) col bezier-steps))
        (when with-bb-lines?
          (draw-facecode-bezier-box-lines (@tiles pos) line-col bezier-steps))
        ;(draw-facecode-color (@tiles pos) col)
                        ))))

