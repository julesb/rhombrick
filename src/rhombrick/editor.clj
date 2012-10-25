(ns rhombrick.editor
  (:use [quil.core]
        [rhombrick.facecode]
        [rhombrick.tiling]
        [rhombrick.tiling-render]
        [rhombrick.button]
        [rhombrick.staticgeometry]))

(def current-tileset (atom #{"001001001001" "000001000001"}))
;(def current-tileset-colors {})

(def button-color {
                   :fill [0 0 0 240]
                   :stroke [0 0 0 0]
                   :fill-hot [128 128 255 192]
                   :stroke-hot [0 0 192 192]
                   :fill-active [255 0 0 192]
                   :stroke-active [255 255 255 192]
                   })

(def editor-state (atom {:level 0
                         :selected [0 0 0]
                         :selected-tilecode-digit []
                         }))

(defn valid-level? [l]
  (and (>= l 0) (< l 3)))

(defn get-level []
  (@editor-state :level))

(defn set-level [l]
  (if (valid-level? l)
    (swap! editor-state assoc :level l)))

(defn get-max-selected-idx []
  (let [level (get-level)]
    (cond 
      (= level 1) 
        (count @current-tileset)
      (= level 2)
        12)))

(defn get-selected [level]
  ((@editor-state :selected) level))

(defn set-selected [i]
  (let [level (get-level)
        selected (get-selected level) ]
  (swap! editor-state assoc :selected (assoc (@editor-state :selected) level i))))


(defn level-up []
  (set-level (inc (@editor-state :level))))

(defn level-down []
  (set-level (dec (@editor-state :level))))

(defn move-left []
  (if (> (get-level) 0)
    (set-selected (mod (dec (get-selected (get-level)))
                       (get-max-selected-idx)))))

(defn move-right []
  (if (> (get-level) 0)
    (set-selected (mod (inc (get-selected (get-level)))
                       (get-max-selected-idx)))))



(defn add-to-current-tileset [code]
  (let [col (compute-tile-color code)]
    (doseq [rc (rotations code)]
      (swap! current-tileset-colors assoc rc col)))
  (swap! current-tileset conj code)
  (init-dead-loci)
  (println "current tileset colors:" @current-tileset-colors))


(defn set-current-tileset [tileset]
  (reset! current-tileset tileset)
  (reset! current-tileset-colors {})
  (doseq [code tileset]
    (let [col (compute-tile-color code)]
      (doseq [rc (rotations code)]
        (swap! current-tileset-colors assoc rc col))
      ;(swap! current-tileset conj code)
      ))
  (init-dead-loci))


(defn remove-from-current-tileset [code]
  (swap! current-tileset-colors dissoc code)
  (swap! current-tileset disj code))


(defn init-editor []
  (reset! current-tileset #{})
  )


(defn draw-selected [[x y] bscale col]
  (apply stroke col)
  (no-fill)
  (rect x y bscale bscale))


(defn draw-facecode-buttons [[x y] sc code parent-idx]
  (let [num-buttons 12
        bspace 1
        bsize (/ (- sc (* bspace (- num-buttons 1)))
                 num-buttons)
        txt-off-x (- (/ bsize 2) 4)
        txt-off-y (+ 5 (/ bsize 2))
        level (get-level)
        selected (get-selected 2)]
    (doseq [i (range num-buttons)]
      (let [bx (+ x (+ (* i bsize) (* i bspace)))
            by y
            tx (+ bx txt-off-x)
            ty (+ by txt-off-y)]
        (if (button bx by bsize bsize button-color (str (.charAt code i)))
          (println "button pressed:" code))
        (if (and (= level 2)
                 (= i selected)
                 (= parent-idx (get-selected 1)))
          (draw-selected [bx by] bsize [255 255 255 255]))
        (fill 255 255 255 255)
        (text (str (.charAt code i)) tx ty)))))


(defn draw-tile-editor [[x y] code bscale parent-idx]
  (let [bx (+ x (/ bscale 2))
        by (+ y (/ bscale 2))
        col [64 64 64 190]]
    (stroke-weight 1)
    (when (button x y bscale bscale button-color code)
      (do
        (println "button pressed:" code)))
    (draw-facecode-buttons [x (+ y bscale 5)] bscale code parent-idx)

    (with-translation [bx by]
        (scale (/ bscale 6))
        (rotate-y (* (frame-count) 0.0051471))
        (no-fill)
        (stroke-weight 1)
        (draw-faces-lite rd-verts rd-faces col)
        (draw-facecode code)
        (scale 2)
        (draw-face-boundaries [0 0 0] code))))



(defn draw-tileset-editor [[x y] tileset bscale]
  (ui-prepare)
  (let [indexed-tileset (into [] (map-indexed #(vec [%1 %2]) tileset))
        level (get-level)
        selected (get-selected 1)]
    (doseq [[i code] indexed-tileset]
      (let [tx (+ x (* i (+ bscale (/ bscale 8))))
            ty y]
        (draw-tile-editor [tx ty] code bscale i)
        (if (and (> level 0) (= i selected))
          (draw-selected [tx ty] bscale [255 255 255 255]))
        )))
  (ui-finish))
      

