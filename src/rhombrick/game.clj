(ns rhombrick.game
  (:use [quil.core]
        [rhombrick.vector]
        [rhombrick.staticgeometry]
        [rhombrick.facecode]
        [rhombrick.tiling]
        [rhombrick.tiling-render]
        [rhombrick.camera]))


(def wait-player? (atom true))
(def selected-pos (atom [0 0 0]))
(def candidates (atom []))
(def selected-candidate-idx (atom 0))


(defn prev-candidate []
  (reset! selected-candidate-idx
          (mod (dec @selected-candidate-idx)
               (count @candidates))))


(defn next-candidate []
  (reset! selected-candidate-idx
          (mod (inc @selected-candidate-idx)
               (count @candidates))))

(defn choose-selected-candidate [_tiles]
  (if (and (> (count @candidates) 0)
           (< selected-candidate-idx (count @candidates)))
    (make-tile _tiles @selected-pos (@candidates selected-candidate-idx))
    _tiles))

(defn set-selected-pos [pos]
  (reset! selected-pos pos))


(defn init-game []
  ;(init-tiler #{})
  (set-selected-pos [0 0 0]) 
  )


(defn start-game [tileset]
  (cancel-tiler-thread)
  (Thread/sleep 100)
  (init-tiler tileset)
  (init-dead-loci!)
  (set-selected-pos [0 0 0])
  (seed-tiler tileset)
  (reset! candidates [])
  (reset! wait-player? true)

  )

(defn draw-selected-pos []
  (let [col [0 255 0]]
    (no-fill)
    (stroke (col 0) (col 1) (col 2) 16)
    (stroke-weight 5)
    (with-translation @selected-pos
      (scale 0.5)
      (draw-faces rd-verts rd-faces [(col 0) (col 1) (col 2) 128]))))


(defn game-step [_tiles tileset]
  (if-let [positions (choose-positions _tiles tileset (get-empty-positions _tiles))]
    (let [new-pos (find-closest-to-center positions)
          neighbourhood (get-neighbourhood _tiles new-pos)
          new-candidates (find-candidates2 neighbourhood tileset)
          ]
      (reset! candidates new-candidates) 
      (set-selected-pos new-pos)
  )))
