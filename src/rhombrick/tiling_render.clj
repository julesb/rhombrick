(ns rhombrick.tiling-render
  (:use [quil.core]
        [rhombrick.vector]
        [rhombrick.staticgeometry]
        [rhombrick.tiling]
        [rhombrick.tilecode]
        [rhombrick.camera]
        [rhombrick.obj-loader]
        [rhombrick.bezierbox :as bbox]
        ;[rhombrick.jigsaw]
        [rhombrick.tile-shape-2d]
        [clojure.math.combinatorics]))


(def rhomb-tex (atom nil))
(def current-tileset-colors (atom {}))
(def model-scale (atom 50))
;(def bezier-box-resolution (atom 8))
;(def bezier-box-control-bias (atom 0.5))
(def bezier-box-smooth-shading? (atom false))
(def bezier-box-line-weight (atom 0.2))
(def face-list (atom #{}))
(def face-id-text (atom []))

(def skybox-tex-idx (atom 0))
(def skybox-textures ["miramar_large.jpg"
                      "stormydays_large.jpg"
                      "grimmnight_large.jpg"
                      "interstellar_large.jpg"
                      "Above_The_Sea.jpg"
                      "cube_filament.jpg"
                      "violentdays_large.jpg"
                      ])

(def bbox-tex-idx (atom 0))
(def bbox-textures ["cave_texture_01-512x512.png"
                    "seamless_texture__circuit_by_ark4n-d2palho.jpg"
                    "seamless_moss_rocks.jpg"
                    "seamless-black-wall-texture-decorating-inspiration-1.jpg"
                    "8-dark-metal-grid-patterns.jpg"
                    "metal_seamless_texture_18_by_jojo_ojoj-d773szm.jpg"
                    "9441717255_5ff5d46373.jpg"
                    "Ceramic_Glaze_Test_Pattern.jpg"
                    "testpattern4po6.png"
                    ])

(def bound-tex-idx (atom 0))
(def bound-texture "cave_texture_01-512x512.png")

(def particle-texture (atom nil))

(def bound-model (get-obj-face-verts-tc (load-obj "data/rhombrick-boundary.obj")))

;(def sky-model (get-obj-face-verts-tc (load-obj "data/rhombic_dodecahedron-tc.obj")))
(def sky-model (get-obj-face-verts-tc (load-obj "data/skysphere.obj")))
(def sky-tex (atom nil))
(def skybox-model (get-obj-face-verts-tc (load-obj "data/skybox.obj")))
(def skybox-tex (atom nil))
(def bbox-tex (atom nil))
(def bound-tex (atom nil))

;(def trunc-oct-model (get-obj-face-verts-tc (load-obj "data/truncated_octahedron.obj")))
(def ^:dynamic trunc-oct-tex)

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


;(defn get-graph-params []
;  [{:title "tiles"
;    :range ((@tiler-state :params) :max-tiles)
;    :data @stats-tile-count}
;   {:title "iter time"
;    :range 500
;    :data @stats-iter-time}
;   {:title "backtrack"
;    :range (max 100 (count @tiles))
;    :data @stats-backtrack}
;   {:title "efficiency"
;    :range 1.0
;    :data @stats-efficiency}
;  ])


;(defn draw-graphs [[x y]]
;  (let [graph-height 100
;        graph-space 5
;        graphs (get-graph-params)]
;    (doseq [i (range (count graphs))]
;      (let [graph (graphs i)
;            gx x
;            gy (+ y (* i graph-height) (* i graph-space))]
;        (draw-graph [gx gy]
;                    stats-buffer-length
;                    graph-height
;                    (graph :title)
;                    (graph :range)
;                    (graph :data))))))
(defn world-to-screen [world-vec]
  [(screen-x (world-vec 0) (world-vec 1) (world-vec 2))
    (screen-y (world-vec 0) (world-vec 1) (world-vec 2))
    0])

(defn facecode-to-hex [code]
  (apply str "0x" (map #(if (= \- %) \0 %) code)))


(defn hsv->rgb [h s v]
  (let [col (int (java.awt.Color/HSBtoRGB h s v))]
    [(bit-shift-right (bit-and col 0x00ff0000) 16)
     (bit-shift-right (bit-and col 0x0000ff00) 8)
     (bit-and col 0x000000ff)]))


(defn phi-palette-color [idx offset]
  (let [phi-1 (- (/ (+ 1 (Math/sqrt 5)) 2) 1)]
    (hsv->rgb
      (mod (+ offset (* idx phi-1)) 1)
      0.75
      1.0)))


(defn make-phi-palette [ncols offset]
  (let [phi-1 (- (/ (+ 1 (Math/sqrt 5)) 2) 1)]
    (->> (range ncols)
         (map #(mod (+ offset (* % phi-1)) 1))
         (map #(hsv->rgb % 0.6 0.6))
         vec)))


(defn get-tile-color [code]
  (get @current-tileset-colors code [64 64 64 128]))


(defn init-tileset-colors [tileset offset]
  (reset! current-tileset-colors {})
  (let [tileset-n (normalize-tileset tileset)
;        offset 0; (/ (mod (tileset-to-number tileset) 255) 255)
        pal (make-phi-palette (count tileset-n) offset)]
    ;(println "tileset-n:" tileset-n)
    (doseq [i (range (count tileset-n))]
      (let [code (tileset-n i)
            col (pal i)]
        (doseq [rc (get-code-symmetries code)]
          (swap! current-tileset-colors assoc rc col))))))

(defn init-tileset-colors-binary [tileset]
  (reset! current-tileset-colors {})
  (let [tileset-n (normalize-tileset tileset)
;        offset 0; (/ (mod (tileset-to-number tileset) 255) 255)
        ;pal (make-phi-palette (count tileset-n) offset)
        ]
    ;(println "tileset-n:" tileset-n)
    (doseq [i (range (count tileset-n))]
      (let [code (tileset-n i)
          col (if (zero? (mod i 2))
                [0 0 0]
                [255 255 255])
            ;col (pal i)
            ]
        (doseq [rc (get-code-symmetries code)]
          (swap! current-tileset-colors assoc rc col))))))


(defn init-tileset-colors-minchange [tileset offset]
  (reset! current-tileset-colors {})
  (let [tileset-n (normalize-tileset tileset)
;        offset 0; (/ (mod (tileset-to-number tileset) 255) 255)
        ;pal (make-phi-palette (count tileset-n) offset)
        ]
    ;(println "tileset-n:" tileset-n)
    (doseq [i (range (count tileset-n))]
      (let [code (tileset-n i)
            col (phi-palette-color (tilecode-to-number (normalize-tilecode code)) 0) ]
        (doseq [rc (get-code-symmetries code)]
          (swap! current-tileset-colors assoc rc col))))))


(defn init-tileset-colors-numcons [tileset offset]
  (reset! current-tileset-colors {})
  (let [tileset-n (normalize-tileset tileset)
        ;tileset-hash (tileset-cons-hash tileset)
;        offset 0; (/ (mod (tileset-to-number tileset) 255) 255)
        ;pal (make-phi-palette (count tileset-n) offset)
        ]
    ;(println "tileset-n:" tileset-n)
    (doseq [i (range (count tileset-n))]
      (let [code (tileset-n i)
            nc (get-num-connected code)
            nccol (phi-palette-color nc 0)
            tilecol (phi-palette-color (tilecode-to-number code) 0)
            col (vec3-add (vec3-scale tilecol 0.5)
                          (vec3-scale nccol 0.5))
            ]
        (doseq [rc (get-code-symmetries code)]
          (swap! current-tileset-colors assoc rc col))))))



(defn get-tileset-group-color [tileset]
  ;(let [tileset-num (/ (tileset-to-binary-number tileset) 4096.0)
  (let [tileset-num (tileset-to-binary-number tileset)
        col (phi-palette-color tileset-num 0) ]
    col
    )
  )


(defn init-tileset-colors-spectrum [tileset offset]
  (reset! current-tileset-colors {})
  (let [tileset-n (normalize-tileset tileset)
        tileset-col (vec3-scale (get-tileset-group-color tileset) 1.0)
;        offset 0; (/ (mod (tileset-to-number tileset) 255) 255)
        ;pal (make-phi-palette (count tileset-n) offset)
        ]
    ;(println "tileset-n:" tileset-n)
    (doseq [i (range (count tileset-n))]
      (let [code (tileset-n i)
            tilenum (tilecode-to-number (normalize-tilecode code))
            div (/ 1.0 4096.0)
            hue (* div tilenum)
            tilecol (phi-palette-color (tilecode-to-number (normalize-tilecode code)) 0)
            col (vec3-scale (vec3-add tilecol tileset-col) 0.5)
            ;col (vec3-scale (vec3-add (hsv->rgb hue 1.0 1.0)
            ;                          tileset-col)
            ;                0.75) 
            ]
        (doseq [rc (get-code-symmetries code)]
          (swap! current-tileset-colors assoc rc col))))))


(defn get-ca-cell-color [code]
  (let [ncons (count (filter #(not= \- %) code))]
    (cond
      (= ncons 4)
        [80 80 80]
      (and (= ncons 6) (= (count (filter #(= \A %) code)) 3))
        [255 255 255]
      (and (= ncons 6) (= (count (filter #(= \B %) code)) 3))
        [0 0 0])))


(defn init-tileset-colors-ca [tileset]
  (reset! current-tileset-colors {})
  (let [tileset-n (normalize-tileset tileset) ]
    (doseq [i (range (count tileset-n))]
      (let [code (tileset-n i)
            col (get-ca-cell-color code) ]
        (doseq [rc (get-code-symmetries code)]
          (swap! current-tileset-colors assoc rc col))))))


(defn draw-obj [faces col]
  (doseq [face faces]
    (let [nvs (count face)]
      (cond
        (= nvs 3)
          (begin-shape :triangles)
        (= nvs 4)
          (begin-shape :quads)
        :else
          (begin-shape))
      (doseq [v face]
        (vertex (v 0) (v 1) (v 2)))
      (end-shape))))


(defn draw-obj-textured [faces tex]
  (no-stroke)
  ;(fill 128 128 128 255)
  (doseq [face faces]
    (let [nvs (count face)]
      (cond
        (= nvs 3)
          (begin-shape :triangles)
        (= nvs 4)
          (begin-shape :quads)
        :else
          (begin-shape))
      (texture-mode :normal)
      (texture tex)
      (doseq [v face]
        (vertex (v 0) (v 1) (v 2) (v 3) (v 4)))
        ;(vertex (v 0) (v 1) (v 2) (* (v 3) 1.0) (* (- 1.0 (v 4)) 1.0)))
        ;(vertex (v 0) (v 1) (v 2) (- 1.0 (v 3)) (- 1.0 (v 4))))
      (end-shape))))

(defn draw-faces [verts faces colors]
  (doseq [i (range (count faces))]
      (let [vert-idx (faces i)
            v0 (verts (vert-idx 0))
            v1 (verts (vert-idx 1))
            v2 (verts (vert-idx 2))
            v3 (verts (vert-idx 3))
            col-idx (mod i (count colors))
            [r g b a] (if (= (count colors) 12) (colors col-idx) colors)]

        ;(if (seq colors)
        ;  (stroke r g b a))
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


(defn draw-verts [verts]
  (doseq [i (range (count verts))]
    (with-translation (verts i)
      (box 0.2))
 ))



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
  (doseq [f (@current-topology :faces)]
    (let [fv (face-idxs-to-verts f)
          fvw (vec (map #(vec3-add pos % ) fv))]
          ;fvw (vec (map #(vec3-add pos (vec3-scale % 0.5)) fv))]
    (if (not (facelist-contains-rotations? fvw))
      (swap! face-list conj fvw)
      (do
        (remove-from-facelist fvw))))))


(defn build-face-list []
  (reset! face-list #{})
  (doseq [tile (keys (@tiler-state :tiles))]
    (add-tile-to-facelist tile)))


(defn build-face-list-ts [ts]
  (reset! face-list #{})
  (doseq [tile (keys (ts :tiles))]
    (add-tile-to-facelist tile)))

;(defn build-face-list2 []
;  (reset! face-list #{})
;  (doseq [tile (@tiler-state :tiles)]
;    (let [code (val tile)
;          connected (set (get-connected-idxs code))
;          non-connected (filter #(not (contains? connected %)) code)
;          faces (face-idxs-to-verts
;          ]
;
;  ))


(defn draw-face-list-new []
  (fill 32 32 32 128)
  ;(fill 32 32 32 128)

  ;(no-fill)
  ;(stroke 0 0 0 192)
  (stroke 40 40 40 190)
  (stroke-weight 4)
  ;(no-stroke)
  (doseq [face-verts @face-list]
    (draw-obj face-verts [])))


(defn draw-face-list []
  ;(fill 0 0 0 64)
  (no-fill)
  ;(fill 32 32 32 128)

  ;(no-fill)
  ;(stroke 0 0 0 192)
  (stroke 64 64 128 240)
  (stroke-weight 0.25)
  ;(no-stroke)
  (doseq [face-verts @face-list]
    (cond
      (= 3 (count face-verts))
        (let [v0 (face-verts 0)
              v1 (face-verts 1)
              v2 (face-verts 2) ]
          ;(stroke 40 40 40 190)
          (begin-shape :triangles)
          ;(texture @rhomb-tex)
          (vertex (v0 0) (v0 1) (v0 2))
          (vertex (v1 0) (v1 1) (v1 2))
          (vertex (v2 0) (v2 1) (v2 2))
          (end-shape)
          ;(stroke 40 40 40 128)
          ;(line (v0 0) (v0 1) (v0 2) (v2 0) (v2 1) (v2 2))
          )
      (= 4 (count face-verts))
        (let [v0 (face-verts 0)
              v1 (face-verts 1)
              v2 (face-verts 2)
              v3 (face-verts 3)]
          ;(stroke 40 40 40 190)
          (begin-shape :quads)
          ;(texture @rhomb-tex)
          (vertex (v0 0) (v0 1) (v0 2))
          (vertex (v1 0) (v1 1) (v1 2))
          (vertex (v2 0) (v2 1) (v2 2))
          (vertex (v3 0) (v3 1) (v3 2))
          (end-shape)
          ;(stroke 40 40 40 128)
          ;(line (v0 0) (v0 1) (v0 2) (v2 0) (v2 1) (v2 2))
          )
      :else
        (do
          (begin-shape)
          (doseq [v face-verts]
            (vertex (v 0) (v 1) (v 2)))
          (end-shape)
          ))
      ))


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
      ;(texture @rhomb-tex)
      (vertex (v0 0) (v0 1) (v0 2) tex-coord-inset 0.5)
      (vertex (v1 0) (v1 1) (v1 2) 0.5 0.0)
      (vertex (v2 0) (v2 1) (v2 2) (- 1.0 tex-coord-inset) 0.5)
      (vertex (v3 0) (v3 1) (v3 2) 0.5 1.0)

      (end-shape)
      ;(line (v0 0) (v0 1) (v0 2) (v2 0) (v2 1) (v2 2))
      )))

; _______________________________________________________________________


(defn draw-selected-tile [pos]
  (let [code ((@tiler-state :tiles) pos)
        col (get-tile-color code)]
    (no-fill)
    (stroke (col 0) (col 1) (col 2) 16)
    (stroke-weight 0.125)
    (with-translation pos
      ;(scale 0.5)
      (draw-faces (@current-topology :verts) (@current-topology :faces) [(col 0) (col 1) (col 2) 128]))))
      ;(draw-faces rd-verts rd-faces [(col 0) (col 1) (col 2) 128]))))


(defn draw-neighbours [pos]
  (no-fill)
  (stroke 64 64 64 64)
  ;(stroke-weight 1)
  (let [ipos (vec (map int pos))]
    (doseq [n (get-neighbours ipos)]
      (with-translation n
        (scale 0.5)
        (draw-faces rd-verts rd-faces nil)
        ))))


(defn draw-face-boundaries-basic [pos]
  (when (contains? (@tiler-state :tiles) pos)
    (stroke-weight 4)
    (stroke 0 0 0 255)
    (fill 255 255 255 255)
    (with-translation pos
      (scale 0.5)
      (doseq [i (range (@current-topology :num-faces))]
        (let [[dx dy dz] ((@current-topology :face-centers) i)
              [dxn dyn dzn] (vec3-normalize [dx dy dz])
              az (Math/atan2 dyn dxn)
              el (- (Math/asin dzn))]
          (with-translation ((@current-topology :face-centers) i)
            (rotate az 0 0 1)
            (rotate el 0 1 0)
            (box 0.2)))))))


(defn draw-face-boundaries [pos ^String code boundary-mode]
  (when (not= boundary-mode :none)
    (when (and (not (nil? code))
               (= (@current-topology :num-faces) (count code))
               (or (contains? (@tiler-state :tiles) pos)
                   (and (= boundary-mode :all)
                        (= (count code) (@current-topology :num-faces)))))
      (let [[r g b] (get-tile-color code)]
        (with-translation pos
          ;(scale 0.5)
          ;(stroke-weight 0.05)
          ;(no-stroke)
          ;(stroke-weight 0.25)
          ;(stroke 128 128 128 255)
          ;(no-stroke)
          ;(stroke r g b 255)
          (assert (and (not (nil? code)) (= (@current-topology :num-faces) (count code))))

          (doseq [^long i (range (@current-topology :num-faces))]
            (when (cond
                    (= boundary-mode :only-empty)
                      (and (is-empty? (@tiler-state :tiles) (get-neighbour-pos pos i))
                           (not= (.charAt code i) \-)
                           (not= (.charAt code i) \0))
                    (= boundary-mode :all)
                      (and (not= (.charAt code i) \-)
                           (not= (.charAt code i) \0))
                    (= boundary-mode :type-change)
                      (and (not= (.charAt code i) \-)
                           (not= (.charAt code i) \0)
                           (not= [r g b] (get-tile-color ((@tiler-state :tiles) (get-neighbour-pos pos i)))))
                    :else
                      false)
              (let [d (.charAt code i)
                    dir ((@current-topology :face-centers) i)
                    [dx dy dz] (vec3-normalize dir)
                    az (Math/atan2 dy dx)
                    el (- (Math/asin dz))
                    thickness (+ 0.075 (* 1.0 (bezier-box-thicknesses (.charAt code i))))
                    bcol (get boundary-colors d [127 127 127])
                    alpha 255]
                (fill (bcol 0) (bcol 1) (bcol 2) alpha)
                ;(stroke (- 255 (bcol 0)) (- 255 (bcol 1)) (- 255 (bcol 2)))
                ;(with-translation (vec3-scale ((@current-topology :face-centers) i) 0.956)
                (with-translation (vec3-sub ((@current-topology :face-centers) i)
                                             (vec3-scale dir  0.04))

                  (rotate az 0 0 1)
                  (rotate el 0 1 0)
                  (rotate (/ PI 2.0) 0 0 1)
                  (if bbox/bezier-box-rotate-45?
                    (rotate (/ PI 4.0) 0 1 0))

                  ;(box 0.0625 thickness thickness))))))))))
                  (scale 0.5)
                  (scale thickness 1.0 thickness)
                  (draw-obj-textured bound-model @bound-tex)
                  ;(box 0.125 thickness thickness)
                  ))))
          (no-tint)
          )))))


(defn draw-face-boundaries-ts [ts pos ^String code boundary-mode]
  (when (and (not (nil? code))
             (= (@current-topology :num-faces) (count code))
             (or (contains? (ts :tiles) pos)
                 (and (= boundary-mode :all)
                      (= (count code) (@current-topology :num-faces)))))
    (let [[r g b] (get-tile-color code)]
      (with-translation pos
        ;(scale 0.5)
        ;(stroke-weight 0.01)
        ;(stroke 128 128 128 255)
        (no-stroke)
        ;(stroke r g b 255)
        (assert (and (not (nil? code)) (= (@current-topology :num-faces) (count code))))

        (doseq [^long i (range (@current-topology :num-faces))]
          (when (cond
                  (= boundary-mode :only-empty)
                    (and (is-empty? (ts :tiles) (get-neighbour-pos pos i))
                         (not= (.charAt code i) \-)
                         (not= (.charAt code i) \0))
                  (= boundary-mode :all)
                    (and (not= (.charAt code i) \-)
                         (not= (.charAt code i) \0))
                  (= boundary-mode :type-change)
                    (and (not= (.charAt code i) \-)
                         (not= (.charAt code i) \0)
                         (not= [r g b] (get-tile-color ((ts :tiles) (get-neighbour-pos pos i)))))
                  :else
                    false)
            (let [d (.charAt code i)
                  dir ((@current-topology :face-centers) i)
                  [dx dy dz] (vec3-normalize dir)
                  az (Math/atan2 dy dx)
                  el (- (Math/asin dz))
                  thickness (+ 0.075 (* 1.0 (bezier-box-thicknesses (.charAt code i))))
                  ;thickness (* 1.3 (bezier-box-thicknesses (.charAt code i)))
                  bcol (get boundary-colors d [127 127 127])
                  ;bcol [96 96 128]; (get boundary-colors d [127 127 127])
                  alpha 255]
              (if (face-digit-like-compatible? d)
                (do ;(fill 160 160 220 alpha)
                    (apply fill bcol)
                    )
                (do
                  (if (>= (int d) 97)
                    (fill 255 255 255 alpha)
                    (fill 0 0 0 alpha))))
              (with-translation (vec3-scale ((@current-topology :face-centers) i) 0.956)
                ;(scale 0.5)
                (rotate az 0 0 1)
                (rotate el 0 1 0)
                (if bbox/bezier-box-rotate-45?
                    (rotate (/ PI 4.0) 0 1 0))
                (box 0.125 thickness thickness)))))))))


(defn draw-billboard [pos tex]
  (let [[dx dy dz] (vec3-normalize (vec3-sub (vec3-scale pos @model-scale)
                                             @camera-pos))
      az (Math/atan2 dy dx)
      el (- (Math/asin dz))]
    (rotate az 0 0 1)
    (rotate el 0 1 0)
    (rotate (/ PI 2.0) 0 0 1)
    (begin-shape :quads)
      (texture tex)
      (vertex -1 0 -1 0 0)
      (vertex  1 0 -1 1 0)
      (vertex  1 0  1 1 1)
      (vertex -1 0  1 0 1)
    (end-shape)))


(defn draw-empty [ts]
  ;(stroke-weight 0.05)
  ;(fill 0 192 0 128)
  ;(no-fill)
  (no-stroke)
  (blend-mode :screen)
;  (hint :disable-depth-test)
;  (tint 255 255 255 240)
  ;(stroke 0 64 0 266)
  ;(doseq [pos (ts :empty)]
  (doseq [pos (sort-by #(- (vec3-distance % @camera-pos)) (ts :empty))]
    ; debugging - highlight empty/tiles conflicts
;    (if (contains? (ts :tiles) pos)
;      (stroke 255 0 0 192)
;      (stroke 0 255 0 32))

    (with-translation pos
       ;(scale 1.0)
       (draw-billboard pos @particle-texture)
      ;(draw-faces (@current-topology :verts) (@current-topology :faces) [255 255 0 128])
      ;(box 0.5 0.5 0.5)
      ))
  (no-tint)
  (blend-mode :blend)
  )


(defn draw-assemblage-center []
  (let [c @assemblage-center]
    (stroke 255 255 64 192)
    (stroke-weight 0.05)
    ;(fill 255 255 0 32)
    (no-fill)
    ;(with-translation (find-assemblage-center @tiles)
    (box 0.5)
    (stroke 255 64 128 192)

    (with-translation c
      (scale 0.5)
      (box 1 1 1))))

; _______________________________________________________________________


(defn draw-axes []
  (no-stroke)
  (with-translation [5 0 0]
    (fill 255 0 0 128)
    (box 10 0.1 0.1))
  (with-translation [0 5 0]
    (fill 0 255 0 128)
    (box 0.1 10 0.1))
  (with-translation [0 0 5]
    (fill 128 128 255 128)
    (box 0.1 0.1 10)))


(defn draw-skysphere [camerapos]
  ;(fill 255 255 255 255)
  (no-stroke)
  (push-matrix)
    ;(rotate (/ Math/PI 2.0) 1.0 0.0 0.0)
    (with-translation camerapos
      (scale 10000 10000 10000)
      (rotate (/ Math/PI 2.0) 1.0 0.0 0.0)
      (draw-obj-textured sky-model @sky-tex)
    )
  (pop-matrix)
  ;(sphere 50) 
  (no-fill)
  )


(defn draw-skybox [camerapos]
  (fill 255 255 255 255)
  (no-stroke)
  (stroke-weight 0.0)
  (texture-wrap :clamp)
  (push-matrix)
    ;(rotate (/ Math/PI 2.0) 1.0 0.0 0.0)
    (with-translation camerapos
      (scale 5000 5000 5000)
      (rotate (/ Math/PI 2.0) 1.0 0.0 0.0)
      (draw-obj-textured skybox-model @skybox-tex)
    )
  (pop-matrix)
  ;(sphere 50) 
  (no-fill)
  )
;(defn draw-curve-tangents [f1-idx f2-idx steps]
;  (when (not= f1-idx f2-idx)
;    (let [step (/ 1.0 steps)]
;      (doseq [s (range steps)]
;        (let [t (* s step)
;              b (bbox/get-bezier-point-3d f1-idx f2-idx t)
;              bt (bbox/get-bezier-tangent-3d f1-idx f2-idx t)
;              bts (vec3-add b (vec3-scale bt 0.25))]
;          (line (b 0) (b 1) (b 2) (bts 0) (bts 1) (bts 2)))))))


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
    (let [[p1 p2 p3 p4] (bbox/get-bezier-controls f1-idx f2-idx)]
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
  (if (= (count connected-idxs) 1)
    (list [(first connected-idxs) (first connected-idxs)])
    (map vec (vec (combinations connected-idxs 2)))))


(defn draw-bezier-box-slow [f1-idx f2-idx f1-scale f2-scale steps]
  (let [[f1-off f2-off] (bbox/get-bezier-anchor-offsets f1-idx f2-idx)
        f1-offsets (map #(vec3-scale % f1-scale) f1-off)
        f2-offsets (map #(vec3-scale % f2-scale) f2-off)
        controls (vec (map #(bbox/get-bezier-controls-with-offset f1-idx f2-idx %1 %2)
                       f1-offsets f2-offsets))]
    (doseq [c1-idx (range (count controls))]
      (let [c2-idx (mod (inc c1-idx) 4)
            c1 (controls c1-idx)
            c2 (controls c2-idx)]
        (begin-shape :triangle-strip)
        (doseq [i (range 0 (inc steps))]
          (let [t (* i (/ 1 steps))
                p1 (bbox/get-bezier-point c1 t)
                p2 (bbox/get-bezier-point c2 t)
                center (bbox/get-bezier-point-3d f1-idx f2-idx t)
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
  (let [[f1-off f2-off] (bbox/get-bezier-anchor-offsets f1-idx f2-idx)
         f1-offsets (map #(vec3-scale % f1-scale) f1-off)
         f2-offsets (map #(vec3-scale % f2-scale) f2-off)
         controls (vec (map #(bbox/get-bezier-controls-with-offset f1-idx f2-idx %1 %2)
                            f1-offsets f2-offsets))]
    (doseq [c controls]
      (draw-curve-with-controls c))))


(defn draw-facecode-shape-2d [code topo res]
  (let [curves (get-facecode-shape-2d code topo res)
        col (get-tile-color code)
        line-col [0 0 0 255]
        ;line-col [255 255 255 255]
        mono-col [255 255 255 255]
        col-transp [(col 0) (col 1) (col 2) 128]
        with-endcaps? true
        final-vert (first (first curves))
        ]
    (no-stroke)
    (stroke-weight 0.0125)
    ;(apply stroke col-transp)
    (apply stroke line-col)

    ;(no-fill)
    (apply fill col-transp)
    ;(apply fill mono-col)
    ;(apply fill col)

    (begin-shape)
    (doseq [curve curves]
      (doseq [[vx vy vz] curve]
        (vertex vx vy vz)))
    (vertex (final-vert 0) (final-vert 1) (final-vert 2))
    (end-shape))
  )

(defn draw-jigsaw-shape-2d [code topo res]
  (let [curves (get-facecode-jigsaw-shape-2d code topo res)
        col (get-tile-color code)
        line-col [0 0 0 255]
        ;line-col [255 255 255 255]
        mono-col [255 255 255 255]
        col-transp [(col 0) (col 1) (col 2) 255]
        with-endcaps? true
        final-vert (first (first curves))
        ]
    ;(println "code/topo/res" code topo res)
    ;(no-stroke)
    (stroke-weight 0.025)
    (apply stroke col-transp)
    ;(apply stroke mono-col)

    ;(no-fill)
    (apply fill col-transp)
    ;(apply fill mono-col)
    ;(apply fill col)
    (when (not= code "------")
      (begin-shape)
      (doseq [curve curves]
        (doseq [[vx vy vz] curve]
          (vertex vx vy vz)))
      (vertex (final-vert 0) (final-vert 1) (final-vert 2))
      (end-shape))))


(defn draw-facecode-inner-shape-2d [code topo res]
  (let [;curves (make-jigsaw-piece code topo res)
        curves (get-facecode-inner-shape-2d code topo res)
        col (get-tile-color code)
        line-col col ;[0 0 0 255]
        ;line-col [255 255 255 255]
        col-transp [(col 0) (col 1) (col 2) 128]
        op-col (vec (map #(- 255 %) col))
        op-col-transp [(op-col 0) (op-col 1) (op-col 2) 128]
        bg-col [16 24 32 255]
        with-endcaps? true
        final-vert (first (first curves))
        ]
    (no-stroke)
    (stroke-weight 0.025)
    (apply stroke op-col-transp)
    ;(stroke 0 0 0 192)

    (no-fill)
    ;(apply fill bg-col)
    ;(no-stroke)
    (begin-shape)
    (doseq [curve curves]
      (doseq [[vx vy vz] curve]
        (vertex vx vy (+ vz 0.1))))
    (vertex (final-vert 0) (final-vert 1) (+ (final-vert 2) 0.1 ))
    (end-shape))
  )


(defn draw-facecode-shape-outline [code topo res]
  (let [curves (get-facecode-shape-2d code topo res)
        col (get-tile-color code)
        ;line-col [0 0 0 128]
        line-col [255 255 255 255]
        col-transp [(col 0) (col 1) (col 2) 192]
         op-col (vec (map #(- 255 %) col))
         
        ]
    ;(no-stroke)

    (stroke-weight 0.05)
    (apply stroke op-col)
    (no-fill)
    ;(apply fill op-col)
    (doseq [curve curves]
      (begin-shape)
      (doseq [[vx vy vz] curve]
        (vertex vx vy vz))
      (end-shape))
  )
  )



(defn draw-facecode-lines [code]
  (let [endpoint-pairs (make-curve-endpoints (get-connected-idxs code))
        num-connected (get-num-connected code)
        ;col (vec3-scale (get-tile-color code) 1.0); (rd-face-colors (mod num-connected 12))
        col (get-tile-color code)
        op-col (vec (map #(- 255 %) col))
;        fill-col (rd-face-colors
;                   (rd-connecting-faces (mod num-connected 12)))
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

    (stroke (col 0) (col 1) (col 2) 192)

    (if (= num-connected 1)
      (let [p ((@current-topology :face-centers) (first (get-connected-idxs code)))]
        (stroke-weight 0.125)
        (line 0 0 0 (p 0) (p 1) (p 2))
        (fill 255 128 128 128)
        ;(box 0.05125 0.05125 0.05125)
        (no-stroke)
        ;(box 0.25 0.25 0.25)
        (no-fill))
        )

    ;(stroke (op-col 0) (op-col 1) (op-col 2) 255)
    (stroke 192 192 255 192)
    ;(stroke-weight 0.45)
    (stroke-weight 0.01215)
    (doseq [endpoints endpoint-pairs]
      (draw-curve (endpoints 0) (endpoints 1)))

    ;draw tangent vectors
    ;(stroke-weight 1)
    ;(stroke 255 120 120 128)
    ;(doseq [endpoints endpoint-pairs]
    ;  (draw-curve-tangents (endpoints 0) (endpoints 1) 3))

    (pop-style)
      ))


(defn draw-facecode-bezier-box-lines [code col steps]
  (when (contains? #{3 4} (count col)) (apply stroke col))
  ;(stroke-weight @bezier-box-line-weight)
  ;(no-fill)
  (doseq [line-verts (bbox/get-bezier-box-lines code steps)]
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
  (doseq [bbox (bbox/get-bezier-box-triangles code steps)]
    (doseq [strip bbox]
      (begin-shape :triangle-strip)
      (doseq [[vx vy vz] strip]
        (vertex vx vy vz))
      (end-shape))))


(defn draw-facecode-bezier-boxes-tex [code col steps]
  ;(fill 255 255 255)
  (fill (col 0) (col 1) (col 2) 255)
  ;(stroke 255 255 255 192)
  (doseq [bbox (bbox/get-bezier-box-triangles code steps)]
    (doseq [strip bbox]
      (let [tx-step (/ 1.0 (count strip))]
        (begin-shape :triangle-strip)
        (texture @bbox-tex)
        (doseq [[i [vx vy vz]] (map-indexed vector strip)]
          (vertex vx vy vz (* i tx-step) (mod i 2)))
        (end-shape)))))


(defn draw-facecode-bezier-boxes-n [code col steps]
  (when (contains? #{3 4} (count col))
    (apply fill col)
    ;(apply stroke col)
    ;(stroke-weight 1)
    )
  ;(stroke 0 0 0 192)
  (no-stroke)
  ;(no-smooth)
  (doseq [bbox (bbox/get-bezier-box-triangles code steps)]
    (doseq [strip bbox]
      (let [verts (vec strip)
            normals (make-strip-vertex-normals verts)]
        (begin-shape :triangle-strip)
        (doseq [i (range (count verts))]
          (let [[vx vy vz] (verts i)
                [nx ny nz] (normals i)]
            (normal nx ny nz)
            (vertex vx vy vz)))
        (end-shape)))))


(defn draw-facecode-bezier-boxes-n2 [code col steps]
  (when (contains? #{3 4} (count col)) (apply fill col))
  ;(stroke 0 0 0 192)
  (no-stroke)
  (doseq [bbox (vec (bbox/get-bezier-box-triangles code steps))]
    (let [normals1 (vec (bbox/get-bezier-box-normals code steps))]
      (doseq [b (range (count bbox))]
         (let [strip (bbox b)
               verts (vec strip)
               normals (vec (normals1 b))]
          (begin-shape :triangle-strip)
          (doseq [i (range (count verts))]
            (let [[vx vy vz] (verts i)
                  [nx ny nz] (normals i)]
              (normal nx ny nz)
              (vertex vx vy vz)))
          (end-shape))))))


(defn make-tube-anchors [f-idx npoints rad ang-offset]
  (let [c ((@current-topology :face-centers) f-idx)
        ;rad 0.25
        vert-ang-off (vec3-angle-between c [0.0 0.0 1.0])
        fake-up-dir (if (or (< vert-ang-off 0.01) (> vert-ang-off 179.99)) [0 1 0] [0 0 1])
        local-x-dir (vec3-cross c fake-up-dir)
        xpos (vec3-add c (vec3-scale local-x-dir rad))
        ang-step (/ (* Math/PI 2) npoints)
        ]
    (vec (map #(rotate-point xpos (vec3-normalize c) (* % ang-step)) (range npoints)))
  ))

(defn make-tube-anchors-for-topology [topo npoints]
  (vec (map #(make-tube-anchors % npoints 0.25 0) (range (topo :num-faces))))
  )

;(defn get-tube-anchors-screen []


(defn draw-tube-anchors [f-idx npoints]
  (let [c ((@current-topology :face-centers) f-idx)
        rad 0.25
        vert-ang-off (vec3-angle-between c [0.0 0.0 1.0])
        fake-up-dir (if (or (< vert-ang-off 0.01) (> vert-ang-off 179.99)) [0 1 0] [0 0 1])
        local-x-dir (vec3-cross c fake-up-dir)
        xpos (vec3-add c (vec3-scale local-x-dir rad))
        ang-step (/ (* Math/PI 2) npoints)
        col (phi-palette-color f-idx 0)
        ]

    (no-stroke)
    (fill 255 255 0)
    ; mark center
    (with-translation c
      (box 0.02 0.02 0.02))

    (doseq [i (range npoints)]
      (let [pos (rotate-point xpos (vec3-normalize c) (* i ang-step))]
        (with-translation pos
          (fill (col 0) (col 1) (col 2))
          (box 0.01))))

  ))


(defn draw-vert-numbers [verts]
  ;(fill 255 255 255 192)
  ;(let [ps (into [] (map world-to-screen verts))]
  (let [ps verts]
    (doseq [i (range (count ps))]
      (with-translation (ps i)
        (text (str i) 0 0)))))


(defn draw-anchor-numbers [verts]
  (doseq [i (range (count verts))]
  ;(doseq [vs verts]
    (let [vs (verts i)
          col (phi-palette-color i 0)]
      (fill (col 0) (col 1) (col 2))
      (draw-vert-numbers vs)
    )
  ))


; bounding sphere
(defn get-assemblage-radius [ts]
  (->> (ts :tiles)
       (keys)
       (map vec3-length)
       (sort)
       (last)
  ))


; returns two opposing corners of axis aligned bounding box
; (there will be a quicker way to do this)
(defn get-assemblage-extents [ts]
  (let [tile-positions (keys (ts :tiles))
        min-xyz [((apply min-key #(% 0) tile-positions) 0)
                 ((apply min-key #(% 1) tile-positions) 1)
                 ((apply min-key #(% 2) tile-positions) 2)]
        max-xyz [((apply max-key #(% 0) tile-positions) 0)
                 ((apply max-key #(% 1) tile-positions) 1)
                 ((apply max-key #(% 2) tile-positions) 2)]]
    [min-xyz max-xyz]))


(defn get-bounding-box-center [ts]
  (apply vec3-bisect (get-assemblage-extents ts)))


(defn get-tiling-dims [ts]
  ;(println "extents:" exts)
  (let [
        exts (get-assemblage-extents ts)
        axlens { :x (+ 1 (Math/abs (- ((exts 0) 0) ((exts 1) 0) )))
                 :y (+ 1 (Math/abs (- ((exts 0) 1) ((exts 1) 1) )))
                 :z (+ 1 (Math/abs (- ((exts 0) 2) ((exts 1) 2) ))) }

        ;ax-sorted (sort-by val axlens)
        ]
    ;(println "axis lengths:" axlens)
    axlens
      ))


(defn draw-aabb [ts]
  (let [ext (get-assemblage-extents ts)
        ;abr (* (vec3-length (first (@current-topology :verts))) 1.0)
        abr (* (@current-topology :aabb-radius) 1.0)
        dims (get-tiling-dims ts)
        dimsa [(+ (dims :x) abr)
               (+ (dims :y) abr)
               (+ (dims :z) abr)]
        ]
    (stroke 255 255 255)
    (no-fill)
    ;(with-translation (vec3-scale (find-assemblage-center (ts :tiles)) 1.0)
      ;(box (dimsa :x) (dimsa :y) (dimsa :z))
      (apply box dimsa)
    ;  )

  ))

(defn draw-tubes [npoints]
  (doseq [i (range (@current-topology :num-faces))]
    (draw-tube-anchors i npoints)
  ))


(defn draw-tiling [ts with-boundaries? with-lines? with-bb-faces? with-bb-lines? boundary-mode]
  (doseq [tile (keys (ts :tiles))]
    (let [pos tile
          code (get (ts :tiles) pos)
          ;col [255 255 255 255]
          ;col [255 255 255 255]
          col (conj (get-tile-color code) 255)
          line-col [(- 255.0 (col 0)) (- 255.0 (col 1)) (- 255.0 (col 2)) 128]
          ;line-col col
          line-col [0 0 0 240]
          ;line-col  [0 0 0 192] ;[192 192 255 192]
          bezier-steps @bbox/bezier-box-resolution]
      (with-translation pos
        ;(scale 0.5)
        ;(stroke-weight 8)
        ;(stroke 0 0 0 64)
        (no-stroke)

        ;(fill 192 192 192 128)
        ;(draw-obj trunc-oct-model [])

        (when with-lines?
          (no-fill)
          (with-translation [0 0 0.0]
          (draw-facecode-lines code)))

        (no-stroke)
        ;(stroke-weight 0.01)
        ;(stroke 0 0 0 192)
        (when with-bb-faces?
          (if @bezier-box-smooth-shading?
            (draw-facecode-bezier-boxes-n (get (ts :tiles) pos) col bezier-steps)
            (draw-facecode-bezier-boxes-tex (get (ts :tiles) pos) col bezier-steps)))
            ;(draw-facecode-bezier-boxes (get (ts :tiles) pos) col bezier-steps)))
        (when with-bb-lines?
          (no-fill)
          (stroke-weight @bezier-box-line-weight)
          (draw-facecode-bezier-box-lines (get (ts :tiles) pos) line-col bezier-steps))

        (when (or (= (@current-topology :id) :hexagon)
                  (= (@current-topology :id) :square))
          (push-matrix)
          (scale 1.0)
          (draw-jigsaw-shape-2d code @current-topology @bezier-box-resolution)
          ;(draw-facecode-shape-2d code @current-topology @bezier-box-resolution)
          (pop-matrix)
          ;(with-translation [0 0 0.05]
          ;  (draw-facecode-inner-shape-2d code @current-topology @bezier-box-resolution))
          ;(draw-facecode-shape-outline code @current-topology @bezier-box-resolution)
          )

        )
      (when with-boundaries?
        (draw-face-boundaries pos code boundary-mode))

      )))


(defn draw-tiling2 [ts attr]
  (init-tileset-colors (get-in ts [:params :tileset]) 0.0)
  (doseq [[pos code] (ts :tiles) ] 
        ;[tile (keys (ts :tiles))]
    (let [;pos tile
          ;code ((ts :tiles) pos)
          ;col [255 255 255 255]
          col (conj (get-tile-color code) 255)
          line-col [(col 0) (col 1) (col 2) 160]

          ;line-col col
          ;line-col  [192 240 255 240]
          bezier-steps (attr :bbox-res)]
      (with-translation pos
        ;(scale 0.5)
        ;(stroke-weight 8)
        ;(stroke 0 0 0 64)
        (no-stroke)
        (when (attr :simple-lines?)
          (no-fill)
          (draw-facecode-lines code))
        (when (attr :bbox-faces?)
          (if (attr :bbox-smooth?)
            (draw-facecode-bezier-boxes-n ((ts :tiles) pos) col bezier-steps)
            (draw-facecode-bezier-boxes ((ts :tiles) pos) col bezier-steps)))
        (when (attr :bbox-lines?)
          ;(reset! bezier-box-line-weight (attr :bbox-line-weight)) ; <-- get rid of
          ;(stroke-weight 0.01)
          (stroke-weight (attr :bbox-line-weight))
          (no-fill)
          (draw-facecode-bezier-box-lines ((ts :tiles) pos) line-col bezier-steps))
        )
      (when (not= (attr :boundary-mode) :none)
        (draw-face-boundaries-ts ts pos code (attr :boundary-mode)))

      )))




(def default-render-attribs {
  :bbox-smooth? true
  :bbox-lines? true
  :bbox-faces? true
  :bbox-line-weight 0.005
  :bbox-res 32
  :simple-lines? false
  :boundary-mode :type-change
                      })


;(defn render [ts attr filename]
;  ;(println (ts :params))
;  ;(println attr)
;  ;(println "iters:" (ts :iters) "tiles:" (count (ts :tiles)))
;  (draw-tiling ts false false false false :type-change)
;  ;(draw-tiling2 ts attr)
;  (println "render" ts attr)
;  (save filename)
;
;  )

