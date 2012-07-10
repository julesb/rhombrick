(ns rhombrick.editor
  (:use [quil.core]
        [rhombrick.facecode]
        [rhombrick.tiling]
        [rhombrick.tiling-render]
        [rhombrick.button]
        [rhombrick.staticgeometry]))

(def current-tileset (atom #{}))

(def editor-visible? (atom false))

(defn add-to-current-tileset [code]
  (swap! current-tileset conj code))

(defn remove-from-current-tileset [code]
  (swap! current-tileset disj code))

(defn init-editor []
  (reset! current-tileset #{}))

(defn set-current-tileset [tileset]
  (reset! current-tileset tileset))

(defn draw-tile-groups []
  )
; _______________________________________________________________________

(defn draw-buttons [pos frame]
  (stroke-weight 1)
  (ui-prepare)
  (doseq [xi (range 10)
          yi (range 10)]
    (let [x (+ (pos 0)
               (* xi button-width)
               (* xi button-space))
          y (+ (pos 1)
               (* yi button-height)
               (* yi button-space))]

      (if (button x y "111111111111")
        (println @ui-state))
      
      (let [bx (+ x (/ button-width 2))
            by (+ y (/ button-width 2))
            code (nth @normalised-facecodes-grouped (+ xi (* yi 10)))]
        (with-translation [bx by]
          (scale (/ button-width 4))
          (rotate-y (* frame 0.051471))
          (stroke 128 128 128 128)
          (no-fill)
          (draw-faces rd-verts rd-faces nil)
          (draw-facecode code)))

      ))
  (ui-finish))

(def buttons-per-row 32)

(defn draw-group-buttons [pos codes frame]
  ;(ui-prepare)
  (doseq [i (range (count codes))]
    (let [x (+ (pos 0)
               (* (int (mod i buttons-per-row))
                  (+ button-width button-space)))
          y (+ (pos 1)
               (* (int (/ i buttons-per-row))
                  (+ button-height button-space)))
          bx (+ x (/ button-width 2))
          by (+ y (/ button-height 2))
          ]
      (button x y "111111111111")
      (with-translation [bx by]
        (scale (/ button-width 4))
        (rotate-y (* frame 0.051471))
        (stroke 128 128 128 128)
        (no-fill)
        (draw-faces rd-verts rd-faces nil)
        (draw-facecode-lite (codes i)))))
  ;(ui-finish)
)
 
(def group-rows {0 0
                 1 2
                 2 4
                 3 6
                 4 8
                 5 12
                 6 18 
                 7 24
                 8 30 
                 9 34
                 10 36
                 11 38
                 12 40})

(defn draw-groups []
  (ui-prepare)
  (doseq [g @normalised-facecodes-grouped]
    (let [i (key g)
          y-offset (+ 32 
                     (* (group-rows i)
                        (+ (/ button-height 2) button-space)))]
        (draw-group-buttons [50 y-offset] (val g) (frame-count))))
  (ui-finish))
      

