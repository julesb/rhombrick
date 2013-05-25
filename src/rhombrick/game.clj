(ns rhombrick.game
  (:use [quil.core]
        [rhombrick.vector]
        [rhombrick.staticgeometry]
        [rhombrick.facecode]
        [rhombrick.tiling :as tiling]
        [rhombrick.tiling-render]
        [rhombrick.camera]
        [ordered.map]))


(def wait-player? (atom true))
(def selected-pos (atom [0 0 0]))
(def candidates (atom []))
(def selected-candidate-idx (atom 0))


(defn prev-candidate []
  (when (> (count @candidates) 0)
    (reset! selected-candidate-idx
            (mod (dec @selected-candidate-idx)
                 (count @candidates)))))


(defn next-candidate []
  (when (> (count @candidates) 0)
    (reset! selected-candidate-idx
            (mod (inc @selected-candidate-idx)
                 (count @candidates)))))


(defn place-selected-candidate []
  (when (and (> (count @candidates) 0)
           (< @selected-candidate-idx (count @candidates)))
    (do
      (make-tile! @selected-pos (@candidates @selected-candidate-idx))
      (update-assemblage-center @tiling/tiles))))


(defn set-selected-pos [pos]
  (reset! selected-pos pos))


(defn init-game []
  ;(init-tiler #{})
  (set-selected-pos [0 0 0]) 
  )


(defn draw-selected-pos []
  (let [col (if (<= (count @candidates) 0) [255 0 0] [0 255 0])]
    (stroke (col 0) (col 1) (col 2) 128)
    (stroke-weight 5)
    ;(fill (col 0) (col 1) (col 2) 16)
    (no-fill)
    (with-translation @selected-pos
      (scale 0.5)
      (draw-faces rd-verts rd-faces [(col 0) (col 1) (col 2) 64]))))


(defn modulate-color [col t]
  (let [n (* (+ (Math/sin (* t 0.00000001)) 1) 0.25)
        s (+ n 0.5)
        col [(int (* (col 0) s))
             (int (* (col 1) s))
             (int (* (col 2) s)) 255]]
    col))


(defn draw-selected-candidate []
  (if (and (> (count @candidates) 0)
           (>= @selected-candidate-idx 0)
           (< @selected-candidate-idx (count @candidates))) 
    (let [pos @selected-pos
          code (@candidates @selected-candidate-idx)
          col (conj (get-tile-color code) 255)
          col-dark [(/ (col 0) 2) (/ (col 1) 2) (/ (col 2) 2) 255]
          col-pulse (modulate-color col (System/nanoTime))
          ;line-col [(col 0) (col 1) (col 2) 255]
          line-col col
          ;line-col  [0 0 0 192] ;[192 192 255 192]
          bezier-steps @bezier-box-resolution]
      (with-translation pos 
        (scale 0.5)
        (when true ; with-lines?
          (no-fill)
          (draw-facecode-lines code))
        (when true ; with-bb-faces?
          (if @bezier-box-smooth-shading?
            (draw-facecode-bezier-boxes-n code col-pulse bezier-steps)
            (draw-facecode-bezier-boxes code col-pulse bezier-steps)))
        (when true ;with-bb-lines?
          (draw-facecode-bezier-box-lines code col-pulse bezier-steps))
        )
      (when true ; with-boundaries?
        (draw-face-boundaries pos code :all))
      
      )))

  


(defn update-game-state [tileset]
  (if-let [positions (get-empty-positions @tiling/tiles)]
    (do
      (set-selected-pos (find-closest-to-center positions))
      (let [neighbourhood (get-neighbourhood @tiling/tiles @selected-pos)
        new-candidates (vec (find-candidates2 neighbourhood tileset))]
        (reset! candidates new-candidates)
        (reset! selected-candidate-idx 0)))))


(defn do-backtrack []
  (reset! tiling/tiles (ordered-map (tiling/backtrack-n @tiles 1)))
  (update-assemblage-center @tiling/tiles))


(defn game-step [tileset]
  (place-selected-candidate)
  (update-game-state tileset))


(defn start-game [tileset]
  (cancel-tiler-thread)
  (Thread/sleep 100)
  (init-tiler tileset)
  (init-dead-loci!)
  (set-selected-pos [0 0 0])
  (seed-tiler tileset)
  (update-game-state tileset)
  (reset! wait-player? true)

  )