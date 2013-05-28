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
(def selected-scale-screen (atom 100))
(def candidates (atom []))
(def candidates-sorted (atom []))
(def selected-candidate-idx (atom 0))
(def neighbour-candidates (atom {}))
(def neighbour-candidates-screen (atom {}))




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


(defn world-to-screen [world-vec]
  [(screen-x (world-vec 0) (world-vec 1) (world-vec 2))
   (screen-y (world-vec 0) (world-vec 1) (world-vec 2))
   0])

(defn update-selected-scale-screen []
  (let [cdir (vec3-normalize (vec3-sub @camera-pos @selected-pos))
        updir @camera-up 
        crossdir (vec3-cross cdir updir)
        offpos (vec3-add @selected-pos crossdir)
        offpos-screen [(screen-x (offpos 0) (offpos 1) (offpos 2))
                       (screen-y (offpos 0) (offpos 1) (offpos 2))
                       0]
        sc (* (vec3-length (vec3-sub offpos-screen @selected-pos-screen)) 0.5)]
    (reset! selected-scale-screen sc)))


(defn update-selected-pos-screen []
  (let [sel-pos [(screen-x (@selected-pos 0) (@selected-pos 1) (@selected-pos 2))
                 (screen-y (@selected-pos 0) (@selected-pos 1) (@selected-pos 2))
                 0]]
    (reset! selected-pos-screen sel-pos)
    (update-selected-scale-screen)))


(defn update-neighbour-candidates [_tiles tileset]
  (if (> (count @candidates) 0)
    (let [tmp-tiles (make-tile _tiles @selected-pos (@candidates @selected-candidate-idx))
          con-nbs (get-empty-connected-neighbours tmp-tiles @selected-pos)
          ;nb-candidates (map #(vec (find-candidates2 (get-neighbourhood tmp-tiles %) tileset))
          ;                   con-nbs) ]
          nb-candidates (into {} (map #(vec [% (vec (find-candidates2 (get-neighbourhood tmp-tiles %) tileset))])
                             con-nbs)) ]
      (reset! neighbour-candidates nb-candidates))
    (reset! neighbour-candidates {})))


(defn update-neighbour-candidates-screen []
  (let [nb-scr (into {} (map #(vec [% (world-to-screen %)])
                             (keys @neighbour-candidates)))]
    (reset! neighbour-candidates-screen nb-scr)))
  

(defn destroy-neighbourhood []
  (let [tmp-tiles @tiling/tiles
        nbs (conj (get-neighbours @selected-pos) @selected-pos)
        new-tiles (apply dissoc @tiling/tiles nbs)]
    (reset! tiling/tiles new-tiles)))


(defn update-game-state [tileset]
  (if-let [positions (get-empty-positions @tiling/tiles)]
    (let [new-selected-pos (find-closest-to-center positions)]
      (set-selected-pos new-selected-pos)
      (let [neighbourhood (get-neighbourhood @tiling/tiles @selected-pos)
        new-candidates (vec (find-candidates2 neighbourhood tileset))]
        (reset! candidates (vec (flatten (vals (group-by get-tile-color new-candidates)))))
        (reset! selected-candidate-idx 0))
      (update-neighbour-candidates @tiling/tiles tileset))))


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


(defn start-game [tileset]
  (cancel-tiler-thread)
  (Thread/sleep 100)
  (init-tiler tileset)
  (init-dead-loci!)
  (set-selected-pos [0 0 0])
  (seed-tiler tileset)
  (update-game-state tileset))


(defn do-backtrack []
  ;(let [last-pos (key (first @tiling/tiles))]
    (reset! tiling/tiles (ordered-map (tiling/backtrack-n @tiles 1)))
    ;(set-selected-pos last-pos)
    (update-assemblage-center @tiling/tiles)
  )


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
  (let [has-candidates? (> (count @candidates) 0)
        col (if has-candidates? [0 255 0] [255 0 0])
        col2 (modulate-color col (System/nanoTime))]
    (stroke (col2 0) (col2 1) (col2 2) 128)
    (stroke-weight 5)
    (if-not has-candidates?
      (fill (col2 0) (col2 1) (col2 2) 32)
      (no-fill))
    (with-translation @selected-pos
      (scale 0.5)
      (draw-faces rd-verts rd-faces [(col2 0) (col2 1) (col2 2) 64]))))


(defn draw-selected-hud []
  (let [has-candidates? (> (count @candidates) 0)
        col (if has-candidates? [0 255 0] [255 0 0])
        col2 (modulate-color col (System/nanoTime))]
    (with-translation @selected-pos-screen
      (stroke (col2 0) (col2 1) (col2 2))
      (stroke-weight 2)
      (no-fill)
      (let [sc 2
            rmin (* sc (- @selected-scale-screen))
            rmax (* sc (* @selected-scale-screen 2))]
        ;(rect rmin rmin rmax rmax)
        (ellipse 0 0 rmax rmax)
        (ellipse 0 0 (+ rmax 30) (+ rmax 30))
        ;(rect -100 -100 200 200)
      )
    )))


(defn draw-neighbour-candidates-hud []
  (stroke 64 0 255 255)
  (stroke-weight 2)
  (fill 64 0 255 192)
  (doseq [nb @neighbour-candidates-screen]
    (let [wpos (key nb)
          cen (val nb)
          cands (@neighbour-candidates wpos)
          num-cands (count cands)
           ]
      (if (> num-cands 0)
        (do
          (with-translation cen
            (no-stroke)
            (fill 0 0 0 128)
            (ellipse 0 0 55 55)
            (doseq [i (range num-cands)]
              (let [ang-step (/ (* Math/PI 2.0) num-cands)
                    ang (* (double i) ang-step)
                    rad 24 
                    px (* (Math/cos ang) rad)
                    py (* (Math/sin ang) rad)
                    code (cands i)
                    col (get-tile-color code) 
                    ]
                (with-translation [px py 0]
                  ;(stroke 64 0 255 255)
                  (stroke-weight 2)
                  (stroke 0 0 0)
                  (fill (col 0) (col 1) (col 2) 255)
                  ;(fill 64 0 255 192)
                  (ellipse 0 0 16 16))))))
        (do
          ; no candidates
          (with-translation cen
            (no-stroke)
            (fill 0 0 0 128)
            (ellipse 0 0 55 55)

            (stroke 255 0 0 128)
            (stroke-weight 8)
            (no-fill)
            (ellipse 0 0 50 50)
          )))
  )))


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
        (draw-face-boundaries pos code :all)))))


(defn render []
  (draw-selected-candidate)
  ;(draw-selected-pos)
  )


(defn render-2d []
  (draw-selected-hud)
  (draw-candidates [(/ (width) 2) 10] @candidates (frame-count))
  (draw-neighbour-candidates-hud)
  )

