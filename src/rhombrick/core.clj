(ns rhombrick.core
  (:use [quil.core]
        [quil.applet]
        [rhombrick.facecode]
        ;[rhombrick.staticgeometry :as geom]
        [rhombrick.tiling]
        [rhombrick.tiling-render]
        [rhombrick.vector]
        [rhombrick.glider]
        [rhombrick.camera]
        [rhombrick.button]
        [rhombrick.editor]
        [clojure.math.combinatorics]
        ;[overtone.osc]
        ))

(import processing.core.PImage)
(import java.awt.Robot)
(def robot (new java.awt.Robot))

(def mouse-position (atom [0 0]))
(def view-scale (atom 1.0))
(def model-scale (atom 50))
;(def frame (atom 0))

;(def num-gliders 50)

(def last-render-time (atom 0))

(def keys-down (atom #{}))

(def mousewarp-pos (atom [716 356])) ; 1440x800
;(def mousewarp-pos (atom [946 506])) ; 1900x1100
(def last-mouse-delta (atom [0 0]))

(def my-applet (atom nil))
(def frame (atom nil))

(def draw-facelist? (atom false))
(def draw-editor? (atom false))
(def draw-gliders? (atom false))


; _______________________________________________________________________


(defn get-location-on-screen []
  (let [p (.getLocationOnScreen @my-applet)
        x (.x p)
        y (.y p)]
    ;(reset! mousewarp-pos [(- (/ (width) 2) (* x 2))
    ;                       (- (/ (height) 2) (* y 2)) ])
    [x y]))

; _______________________________________________________________________
; Rendering and events 


; _______________________________________________________________________


(defn setup []
    ;(println "applet:" my-applet)
     
    ;(reset! rhomb-tex (load-image "cave_texture_01-512x512.png"))
    ;(reset! rhomb-tex (load-image "testpattern4po6.png"))
    (reset! rhomb-tex (load-image "gradient.jpg"))
    ;(reset! rhomb-tex (load-image "map-15Bsubset.jpg"))

    ;(println "texture:" @rhomb-tex)
    (texture-mode :normalized)
      
    ;(println (.frame (current-applet)))
    ;(println "screen pos" (get-location-on-screen))
    ;(println "screen size:" (width) (height))
    ;(println "frame:" (.getLocation (.frame @my-applet)))
    ;(.mouseMove robot 0 0)
    ;(.mouseMove robot (/ (width) 2) (/ (height) 2))
    
    (smooth)
    (frame-rate 60)
    (update-camera)
    (println "setting font")
    (text-font (load-font "FreeMono-16.vlw"))
    (set-state! :mouse-position (atom [0 0]))
    (println "building normalised faced set")
    (build-normalised-facecode-set)
    (println "initialising tiler")
    (init-tiler @current-tileset)
    ;(make-tiling-iteration) ; needed so init-gliders works
    ;(make-backtracking-tiling-iteration @current-tileset)
    (init-gliders num-gliders)
    ;(println @gliders)
    ;(init-todo)

;    (doseq [val (range 10)]
;      (osc-send client "/test" "i" (float val)))
    ;(reset! mousewarp-pos [(mouse-x) (mouse-y)])
    ;(println "mouse:" @mousewarp-pos)

    ;(println "bezier test: " (bezier-point 1.0 2.0 3.0 4.0 0.5))
    (println "setup done")
    )
    ;(doseq [code @normalised-facecodes]
    ; (println code))
    ;(println "total" (count @normalised-facecodes) "facecodes"))

; _______________________________________________________________________

(defn draw-info [x y]
  (let [line-space 20
        lines [(str "tileset: " @current-tileset)
               (str "  state: " @tiler-state)
               (str "  iters: " @tiler-iterations)
               (str "  tiles: " (count @tiles) "/" @max-tiles) 
               (str "  empty: " (count @empty-positions)) 
               (str "   dead: " (count @dead-loci))
               (str "-------------")
               (str "  scale: " @model-scale)
               (str " radius: " @assemblage-max-radius)
               (str "    fps: " (int (current-frame-rate)))
               ]]
    (fill 255 255 255 255)
    (doseq [i (range (count lines))]
      (text (lines i) x (+ y (* i line-space))))))

; _______________________________________________________________________



(defn draw-xyplane []
  (quad -1 -1
         1 -1
         1  1
        -1  1))

; _______________________________________________________________________

(defn mouse-delta-old [scale-factor]
  (let [dx (- (mouse-x) (pmouse-x))
        dy (- (mouse-y) (pmouse-y))]
    (vec3-scale [dx dy 0] scale-factor)))


(defn mouse-delta [scale-factor]
  (let [dx (- (mouse-x) (@mousewarp-pos 0))
        dy (- (mouse-y) (@mousewarp-pos 1))]
    (vec3-scale [dx dy 0] scale-factor)))


; _______________________________________________________________________


(def key-command-map
  {
   \, #(do
         (swap! model-scale + 1.0)
         (println "model-scale: " @model-scale))
   \. #(do
        (swap! model-scale - 1.0)
        (println "model-scale: " @model-scale))
   ;\r #(make-cubic-tiling 10 10 10)
   \r #(do
         (soft-init-tiler @current-tileset)
         (make-backtracking-tiling-iteration2 @tiles @current-tileset)
         (init-gliders num-gliders))
   \R #(do 
         (init-tiler @current-tileset)
         (set-current-tileset (get-random-tileset))
         (println "random tileset:" @current-tileset) 
         (make-backtracking-tiling-iteration2 @tiles @current-tileset)
         (init-gliders num-gliders)
         )
   \- #(do 
         (swap! camera-fov - 1)
         (update-camera))
   \= #(do
         (swap! camera-fov + 1)
         (update-camera))
   \c #(do
         (let [m (mod (inc @camera-mode) camera-num-modes)]
         (reset! camera-mode m)
         (if (= @camera-mode 2)
           (no-cursor)
           (cursor))))
    \f #(do
         (swap! draw-facelist? not)
         (when @draw-facelist?
           (build-face-list))
         (println "draw facelist? " @draw-facelist?))
    \` #(do
         (swap! draw-editor? not)
         (if @draw-editor?
           (cursor))
         (println "draw editor? " @draw-editor?))
    \g #(do
          (swap! draw-gliders? not)
          (if draw-gliders?
            (init-gliders num-gliders)))
    \T #(do 
         (swap! max-tiles inc)
         (println "max tiles:" @max-tiles))
    \t #(do 
         (swap! max-tiles dec)
         (println "max tiles:" @max-tiles))
    \[ #(do
          (swap! adhd - 0.1)
          (println "adhd:" @adhd "auti:" @autism))
    \] #(do
          (swap! adhd + 0.1)
          (println "adhd:" @adhd "auti:" @autism))
    \{ #(do
          (swap! autism - 0.1)
          (println "adhd:" @adhd "auti:" @autism))
    \} #(do
          (swap! autism + 0.1)
          (println "adhd:" @adhd "auti:" @autism))
   })


(def key-movement-map
  {
   \w #(do
         (let [dir (get-camera-dir)]
           (swap! camera-pos vec3-add (vec3-scale dir 10.0))))
   \s #(do
         (let [dir (get-camera-dir)]
           (swap! camera-pos vec3-sub (vec3-scale dir 10.0))))
   \a #(do
         (let [dir (get-camera-x-dir)]
           (swap! camera-pos vec3-sub (vec3-scale dir 10.0))))
   \d #(do
         (let [dir (get-camera-x-dir)]
           (swap! camera-pos vec3-add (vec3-scale dir 10.0))))
   })



(defn do-movement-keys []
  (doseq [k @keys-down]
    (when (contains? key-movement-map k)
      ((key-movement-map k)))))
    

(defn key-typed []
  (let [keychar (raw-key)]
    (when (contains? key-command-map keychar)
      ((key-command-map keychar)))))


(defn key-pressed []
  (swap! keys-down conj (raw-key))
  ;(do-movement-keys)
  )


(defn key-released []
  (swap! keys-down disj (raw-key))
  ;(do-movement-keys)
  )

; _______________________________________________________________________


(defn mouse-moved []
  (let [x (mouse-x) y (mouse-y)
        delta [(- (mouse-x) (@mousewarp-pos 0))
               (- (mouse-y) (@mousewarp-pos 1)) 0]]
    (when (not @draw-editor?)
      (reset! last-mouse-delta (vec3-scale delta 0.01))
      (reset! (state :mouse-position) [x y]))
    
    (when @draw-editor?
      (update-ui-state :mouse-x (mouse-x))
      (update-ui-state :mouse-y (mouse-y)))
    ))


(defn mouse-pressed []
  ;(println "mouse-pressed")
  (update-ui-state :mouse-down true))


(defn mouse-released []
  ;(println "mouse-released")
  (update-ui-state :mouse-down false))
  
; _______________________________________________________________________


(defn draw []
  ;(get-location-on-screen)
  (let [frame-start-time (System/nanoTime)]
  (do-movement-keys)

  ;(when (= 0 (mod (frame-count) 1))
  ;  (make-backtracking-tiling-iteration @current-tileset))
 
  (when (= @tiler-state :running)
    (if (and (> (count @empty-positions) 0)
             (> (count @current-tileset) 0))
      (make-backtracking-tiling-iteration2 @tiles @current-tileset)
      (do
        (build-face-list)
        (halt-tiler))))


  (when @draw-gliders?   
    (update-gliders))
    
  (background 32 32 64)
  ;(background 0 0 0)

  (push-matrix)

  (cond
    (= @camera-mode 0)
    ; rubber band camera to glider
      (do
        (let [g (vec3-scale (get-glider-pos 1) @model-scale)
              d (dist (@camera-pos 0)
                      (@camera-pos 1)
                      (@camera-pos 2)
                      (g 0) (g 1) (g 2))
              dir (vec3-normalize (vec3-sub g @camera-pos))
              newpos (vec3-add @camera-pos (vec3-scale dir (* d 0.030)))
              cl-d (dist (@camera-lookat 0)
                         (@camera-lookat 1)
                         (@camera-lookat 2)
                       (g 0) (g 1) (g 2))
              cl-dir (vec3-normalize (vec3-sub g @camera-lookat))
              new-camera-lookat (vec3-add @camera-lookat 
                                          (vec3-scale cl-dir
                                                      (* cl-d 0.15)))]
          (reset! camera-lookat new-camera-lookat)    
          (reset! camera-pos newpos)
          (camera (newpos 0) (newpos 1) (+ (newpos 2) 10)
                  (new-camera-lookat 0)
                  (new-camera-lookat 1)
                  (new-camera-lookat 2)
                  0 0 -1)))
    (= @camera-mode 1)
    ; camera follows paths
      (do
        (if (> (count @gliders) 1)
          (let [cam-pos (vec3-scale (get-glider-pos 1) @model-scale)
                cam-lookat (vec3-scale (get-glider-nextpos 1) @model-scale)]
            (camera (cam-pos 0) (cam-pos 1) (- (cam-pos 2) 1)
                    (cam-lookat 0) (cam-lookat 1) (- (cam-lookat 2) 1)
                    0 0 1))))
    (= @camera-mode 2)
    ; mouse/keyboard camera control
      (do
          (let [md @last-mouse-delta ]
            ;(println "mouse delta: " md)
            (do-camera-transform @camera-pos
                                 (* 1.0 (md 1))
                                 (* -1.0 (md 0)))
            (if (not @draw-editor?)
              (do
                (reset! last-mouse-delta (mouse-delta 0.0001))
                (.mouseMove robot (/ (width) 2) (/ (height) 2))))))
  )

  (perspective (radians @camera-fov) 
                 @camera-aspect-ratio
                 @camera-near-clip
                 @camera-far-clip)

  (let [[mx my] @(state :mouse-position)]
    (push-matrix)
    (scale @model-scale)

    ;(stroke 0 255 255 128)
    ;(stroke-weight 1)
    ;(no-fill)
    ;(box 10 10 10)
    
    (when @draw-gliders?
      (draw-gliders (frame-count))
    )
    
    (lights)
    ;(light-falloff 1.0 0.2 0.0)
    ;(ambient-light 64 64 64)


    ;(hint :disable-depth-test) 
    (draw-tiling)
    ; (hint :enable-depth-test)

    (when @draw-facelist?
      (draw-face-list))
      ;(draw-face-list-textured))


    ;(when (seq @empty-positions)
    ;  (draw-empty))


    ;(lights)
    (when @draw-gliders?
      (let [selected-tile ((get-glider 1) :current-tile)
            tile-color (get-group-color selected-tile)]
        (draw-neighbours selected-tile)
        ;(draw-curve-boundary-points selected-tile)
        (draw-selected-tile selected-tile)
        ;(point-light (tile-color 0) (tile-color 1) (tile-color 2)
        ;             (selected-tile 0) (selected-tile 1) (selected-tile 2))
        ))
    (pop-matrix)
  )

  (pop-matrix)
  
  ; 2d hud stuff
  ;(hint :disable-depth-test)
  ;(camera)
  ;(ortho)
  (hint :disable-depth-test)
  (draw-info 10 40)
  (camera)


  (when @draw-editor?
    (draw-tileset-editor [1285 20] @current-tileset 140))


  (hint :enable-depth-test)


  (reset! last-render-time
          (float (/ (- (System/nanoTime)
                       frame-start-time)
                    1000000.0)))

  (if (and (= @tiler-state :running)
           (= (mod (frame-count) 150) 0))
   (println "frame-time: " @last-render-time
            "tiles:" (count @tiles)
            "adhd:" @adhd "auti:" @autism
            ))
  ))




; _______________________________________________________________________


(defn -main [& args] 
  (defsketch rhombrick 
    :title "rhombrick"
    :setup setup 
    :draw draw
    ;:size [1900 1100]
    :size [1440 800]
    :renderer :opengl 
    :key-typed key-typed
    :key-pressed key-pressed
    :key-released key-released
    :mouse-moved mouse-moved
    :mouse-pressed mouse-pressed
    :mouse-released mouse-released))

;(reset! frame ((current-applet) meta :target-obj deref))

;(sketch-start rhombrick)

;(reset! my-applet rhombrick)
