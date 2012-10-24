(ns rhombrick.editor
  (:use [quil.core]
        [rhombrick.facecode]
        [rhombrick.tiling]
        [rhombrick.tiling-render]
        [rhombrick.button]
        [rhombrick.staticgeometry]))

(def current-tileset (atom #{"001001001001" "000001000001"}))
;(def current-tileset-colors {})

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


(defn draw-facecode-buttons [[x y] sc code]
  (let [num-buttons 12
        bspace 1
        bsize (/ (- sc (* bspace (- num-buttons 1)))
                 num-buttons)
        txt-off-x (- (/ bsize 2) 4)
        txt-off-y (+ 5 (/ bsize 2))]
    (doseq [i (range num-buttons)]
      (let [bx (+ x (+ (* i bsize) (* i bspace)))
            by y
            tx (+ bx txt-off-x)
            ty (+ by txt-off-y)]
        (if (button bx by bsize bsize (str (.charAt code i)))
          (println "button pressed:" code))
        (fill 255 255 255 255)
        (text (str (.charAt code i)) tx ty)))))


(defn draw-tile-editor [[x y] code bscale]
  (let [bx (+ x (/ bscale 2))
        by (+ y (/ bscale 2))
        col [64 64 64 190]]
    (stroke-weight 1)
    (when (button x y bscale bscale code)
      (do
        (println "button pressed:" code)))
    (draw-facecode-buttons [x (+ y bscale)] bscale code)

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
  (let [indexed-tileset (into [] (map-indexed #(vec [%1 %2]) tileset))]
    (doseq [[i code] indexed-tileset]
      (let [tx x
            ty (+ y (* i (+ bscale (/ bscale 8))))]
        (draw-tile-editor [tx ty] code bscale))))
  (ui-finish))
      

