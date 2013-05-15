(ns rhombrick.game
  (:use [quil.core]
        [rhombrick.vector]
        [rhombrick.staticgeometry]
        [rhombrick.facecode]
        [rhombrick.tiling]
        [rhombrick.tiling-render]
        [rhombrick.camera]))



(def selected-pos (atom [0 0 0]))

(defn set-selected-pos [pos]
  (reset! selected-pos pos))


(defn init-game []
  (set-selected-pos [0 0 0]) 
  )


(defn draw-selected-pos []
  (let [col [0 255 0]]
    (no-fill)
    (stroke (col 0) (col 1) (col 2) 16)
    (stroke-weight 5)
    (with-translation @selected-pos
      (scale 0.5)
      (draw-faces rd-verts rd-faces [(col 0) (col 1) (col 2) 128]))))
