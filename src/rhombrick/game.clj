(ns rhombrick.game
  (:use [quil.core]
        [rhombrick.vector]
        [rhombrick.staticgeometry]
        [rhombrick.facecode]
        [rhombrick.tiling :as tiling]
        [rhombrick.tiling-render]
        [rhombrick.camera]
        [rhombrick.editor]
        [rhombrick.button]
        [ordered.map]))


(def selected-pos (atom [0 0 0]))
(def selected-pos-screen (atom [0 0 0]))
(def candidates (atom []))
(def candidates-sorted (atom []))
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


(defn update-selected-pos-screen []
  (reset! selected-pos-screen 
          [(screen-x (@selected-pos 0) (@selected-pos 1) (@selected-pos 2))
           (screen-y (@selected-pos 0) (@selected-pos 1) (@selected-pos 2))]))


(defn update-game-state [tileset]
  (if-let [positions (get-empty-positions @tiling/tiles)]
    (do
      (set-selected-pos (find-closest-to-center positions))
      (reset! selected-pos-screen 
              [(screen-x (@selected-pos 0) (@selected-pos 1) (@selected-pos 2))
               (screen-y (@selected-pos 0) (@selected-pos 1) (@selected-pos 2))])

      (let [neighbourhood (get-neighbourhood @tiling/tiles @selected-pos)
        new-candidates (vec (find-candidates2 neighbourhood tileset))]
        (reset! candidates (vec (flatten (vals (group-by get-tile-color new-candidates)))))
        (reset! selected-candidate-idx 0)))))


(defn start-game [tileset]
  (cancel-tiler-thread)
  (Thread/sleep 100)
  (init-tiler tileset)
  (init-dead-loci!)
  (set-selected-pos [0 0 0])
  (seed-tiler tileset)
  (update-game-state tileset))


(defn do-backtrack []
  (reset! tiling/tiles (ordered-map (tiling/backtrack-n @tiles 1)))
  (update-assemblage-center @tiling/tiles))


(defn game-step [tileset]
  (place-selected-candidate)
  (update-game-state tileset))


(defn modulate-color [col t]
  (let [n (* (+ (Math/sin (* t 0.00000001)) 1) 0.25)
        s (+ n 0.5)
        col [(int (* (col 0) s))
             (int (* (col 1) s))
             (int (* (col 2) s)) 255]]
    col))


(defn draw-selected-pos []
  (let [col (if (<= (count @candidates) 0) [255 0 0] [0 255 0])]
    (stroke (col 0) (col 1) (col 2) 128)
    (stroke-weight 5)
    ;(fill (col 0) (col 1) (col 2) 16)
    (no-fill)
    (with-translation @selected-pos
      (scale 0.5)
      (draw-faces rd-verts rd-faces [(col 0) (col 1) (col 2) 64]))))


(defn draw-selected-hud []
    (with-translation @selected-pos-screen
      (stroke 255 255 0)
      (stroke-weight 8)
      (no-fill)
      (rect -100 -100 200 200)
    ))


(defn draw-candidates [pos codes frame]
  ;(ui-prepare)
  (doseq [i (range (count codes))]
    (let [code (codes i)
          bscale 64
          buttons-per-row 12
          x (pos 0)
          y (pos 1)
          bx (+ x (/ button-width 2))
          by (+ y (/ button-height 2))
          tx (+ x (* (mod i buttons-per-row) (+ bscale (/ bscale 16))))
          ty (+ y (* (int (/ i buttons-per-row)) (+ bscale (/ bscale 16))))
          col (get-tile-color code)
          col2 col
          ]
      ;(if (button x y bscale bscale col "------------")
      ;  (do
      ;    (println "pressed")
      ;    ))

      (with-translation [bx by]
        ;(scale (/ button-width 4))
        (rotate-y (* frame 0.051471))
        ;(apply stroke col)
        (no-fill)
        (stroke-weight 2))
        (draw-faces-lite rd-verts rd-faces col)
        (draw-tile-button [tx ty] (codes i) bscale i)))
        ;(draw-facecode-bezier-boxes (codes i) col 8)))
        ;(draw-facecode-lite (codes i))))
  ;(ui-finish)
)


(defn draw-selected-candidate []
  (if (and (> (count @candidates) 0)
           (>= @selected-candidate-idx 0)
           (< @selected-candidate-idx (count @candidates))) 
    (let [pos @selected-pos
          code (@candidates @selected-candidate-idx)
          col (conj (get-tile-color code) 255)
          ;col (conj (vec (get-tile-color code)) 255)
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


(defn render []
  (draw-selected-candidate)
  (draw-selected-pos))


(defn render-2d []
  (draw-selected-hud)
  (draw-candidates [(/ (width) 2) 10] @candidates (frame-count))
  )

