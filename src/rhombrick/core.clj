(ns rhombrick.core
  (:use [quil.core]
        [quil.applet]
        [rhombrick.facecode]
        [rhombrick.staticgeometry]
        [rhombrick.tiling]
        [rhombrick.tiling-render]
        [rhombrick.vector]
        [rhombrick.glider]
        [rhombrick.camera]
        
        ;[overtone.osc]
        )
  (:gen-class))

(import processing.core.PImage)
(import java.awt.Robot)
(def robot (new java.awt.Robot))

(def mouse-position (atom [0 0]))
(def view-scale (atom 1.0))
(def model-scale (atom 200))
;(def frame (atom 0))

(def num-gliders 50)

(def last-render-time (atom 0))

(def keys-down (atom #{}))

(def mousewarp-pos (atom [946 506]))
(def last-mouse-delta (atom [0 0]))

(def my-applet (atom nil))

(def draw-facelist? (atom false))
(def draw-editor? (atom false))
  
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
    (println "applet:" my-applet)
     
    ;(reset! rhomb-tex (load-image "cave_texture_01-512x512.png"))
    ;(reset! rhomb-tex (load-image "testpattern4po6.png"))
    (reset! rhomb-tex (load-image "gradient.jpg"))
    (println "texture:" @rhomb-tex)
    (texture-mode :normalized)
      
    ;(println (.frame (current-applet)))
    ;(println "screen pos" (get-location-on-screen))
    (println "screen size:" (width) (height))
    ;(println "frame:" (.getLocation (.frame @my-applet)))
    ;(.mouseMove robot 0 0)
    ;(.mouseMove robot (/ (width) 2) (/ (height) 2))
    
    (smooth)
    (frame-rate 60)
    (sphere-detail 12)
    (update-camera)
    ;(display-filter :blur 10)
    (text-font (load-font "FreeMono-16.vlw"))
    ;(text-mode :screen)
    (set-state! :mouse-position (atom [0 0]))
    (build-normalised-facecode-set)
    ;(make-cubic-tiling 10 10 10)
    ;(reset! tiles {})
    (init-tiler)
    ;(make-tiling-iteration) ; needed so init-gliders works
    (make-backtracking-tiling-iteration)
    (init-gliders num-gliders)
    ;(println @gliders)
    ;(init-todo)

;    (doseq [val (range 10)]
;      (osc-send client "/test" "i" (float val)))
    ;(reset! mousewarp-pos [(mouse-x) (mouse-y)])
    (println "mouse:" @mousewarp-pos)

    (println "bezier test: " (bezier-point 1.0 2.0 3.0 4.0 0.5))
    )
    ;(doseq [code @normalised-facecodes]
    ; (println code))
    ;(println "total" (count @normalised-facecodes) "facecodes"))

; _______________________________________________________________________

(defn draw-info []
  (fill 255 255 255 255)
  (text (str "fps: " (current-frame-rate)) 10 40))

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
         (init-tiler)
         ;(make-tiling-iteration)
         (make-backtracking-tiling-iteration)
         (init-gliders num-gliders))
   \R #(do 
         (init-tiler)
         (random-tileset)
         ;(make-tiling-iteration)
         (make-backtracking-tiling-iteration)
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
         ;(if (= @camera-mode 2)
         ;  (no-cursor)
         ;  (cursor)))
        ))
    \f #(do
         (swap! draw-facelist? not)
         (println "draw facelist? " @draw-facelist?))
    \` #(do
         (swap! draw-editor? not)
         (println "draw editor? " @draw-editor?))

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
    (if (contains? key-movement-map k)
      (do
        ((key-movement-map k))))))
    

(defn key-typed []
  (let [keychar (raw-key)]
    (if (contains? key-command-map keychar)
      ((key-command-map keychar)))
    ;(println keychar)
    ))


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
    (reset! last-mouse-delta (vec3-scale delta 0.01))
    (reset! (state :mouse-position) [x y])))



; _______________________________________________________________________


(defn draw []
  ;(println "applet:" (.getLocationOnScreen @my-applet))
  ;(get-location-on-screen)
  (let [frame-start-time (System/nanoTime)]
  ;(let [frame-start-time (millis)]
  (do-movement-keys) 
  ;(make-tiling-iteration)
  (make-backtracking-tiling-iteration)
  ;(auto-seed-todo)
;  (if (= (count @todo) 0)
;    (do
;      (init-tiler)
;      (make-tiling-iteration)
;      (init-gliders num-gliders)
;      ;(random-tileset)
;      ))

;  (if (> (count @tiles) max-tiles)
;    (do
;      (init-tiler)
;      (make-tiling-iteration)
;      (init-gliders num-gliders)
;    ))

  (update-gliders)
  ;(background 32 32 192)
  (background 0 0 0)
  ;(lights)

 ;attach cam to glider 1, lookat glider 2
 ; (if (> (count @gliders) 1)
 ;   (let [cam-pos (vec3-scale (get-glider-pos 1) @model-scale)
 ;         cam-lookat (vec3-scale (get-glider-pos 2) @model-scale) ]
 ;     (camera (cam-pos 0) (cam-pos 1) (- (cam-pos 2) 10)
 ;             (cam-lookat 0) (cam-lookat 1) (cam-lookat 2)
 ;             0 0 1)))
   
(cond
  ; rubber band camera to glider
  (= @camera-mode 0)
    (do
      (let [g (vec3-scale (get-glider-pos 1) @model-scale)
            d  (dist (@camera-pos 0)
                     (@camera-pos 1)
                     (@camera-pos 2)
                     (g 0) (g 1) (g 2))
            dir (vec3-normalize (vec3-sub g @camera-pos))
            newpos (vec3-add @camera-pos (vec3-scale dir (* d 0.020)))
            cl-d (dist (@camera-lookat 0)
                       (@camera-lookat 1)
                       (@camera-lookat 2)
                     (g 0) (g 1) (g 2))
            cl-dir (vec3-normalize (vec3-sub g @camera-lookat))
            new-camera-lookat (vec3-add @camera-lookat 
                                        (vec3-scale cl-dir
                                                    (* cl-d 0.1)))]
        (reset! camera-lookat new-camera-lookat)    
        (reset! camera-pos newpos)
        (camera (newpos 0) (newpos 1) (- (newpos 2) 20)
                (new-camera-lookat 0)
                (new-camera-lookat 1)
                (new-camera-lookat 2)
                0 0 1)))
  ; camera follows paths
  (= @camera-mode 1)
    (do
      (if (> (count @gliders) 1)
        (let [cam-pos (vec3-scale (get-glider-pos 1) @model-scale)
              cam-lookat (vec3-scale (get-glider-nextpos 1) @model-scale) ]
          (camera (cam-pos 0) (cam-pos 1) (- (cam-pos 2) 1)
                  (cam-lookat 0) (cam-lookat 1) (- (cam-lookat 2) 1)
                  0 0 1))))
  ; mouse/keyboard camera control
  (= @camera-mode 2)
    (do
      (let [md @last-mouse-delta ]
        ;(println "mouse delta: " md)
        (do-camera-transform @camera-pos
                             (* 1.0 (md 1))
                             (* -1.0 (md 0)))
        (reset! last-mouse-delta (mouse-delta 0.0001)))
      (.mouseMove robot (/ (width) 2) (/ (height) 2)))

  )

  (perspective (radians @camera-fov) 
                 @camera-aspect-ratio
                 @camera-near-clip
                 @camera-far-clip)
  ;(lights)  
  ;(light-falloff 0.5 0.0 0.0) 
  ;(light-specular 255 0 0)
  (stroke 0 255 255 128)
  (stroke-weight 1)
  (no-fill)
  ;(sphere 2000)
  (box 2000 2000 2000)
 

  (let [[mx my] @(state :mouse-position)]
    (push-matrix)
    (scale @model-scale)
    ;(rotate-x (* (- my 400) -0.01))
    ;(rotate-y (* (- mx 700) 0.01))
    ;(rotate-x (* @frame 0.00351471))
    ;(rotate-y (* @frame 0.0035236))
    ;(rotate-z (* @frame 0.0035123))
    
    ;(build-face-list) 
    ;(draw-face-list)
    
    (stroke 255 255 255 192)
    (stroke-weight 1)
    
    ;(draw-gliders (frame-count))
    
    (lights)
    ;(light-falloff 1.0 0.2 0.0)
    ;(ambient-light 64 64 64)
    (when @draw-facelist?
      (draw-face-list-textured))

    (draw-tiling)

    ;(no-fill)
    ;(stroke-weight 1)
    ;(draw-todo)

    (fill 192 192 192 255)
    ;(if (seq @todo)
    ;  (draw-todo-head))

    (if (seq @empty-positions)
      (draw-empty))


  ;(lights) 
;      (let [selected-tile ((get-glider 1) :current-tile)
;            tile-color (get-group-color selected-tile)]
;        ;(draw-neighbours selected-tile)
;        (draw-curve-boundary-points selected-tile)
;        ;(draw-selected-tile selected-tile)
;        ;(point-light (tile-color 0) (tile-color 1) (tile-color 2)
;        ;                (selected-tile 0) (selected-tile 1) (selected-tile 2))
;        )
  

    (pop-matrix)
    (if @draw-editor?  
      (do
      (no-fill)
      (stroke-weight 1)
      (stroke 255 255 255 255)
      (draw-normalized-facecodes (frame-count))))


  )
;  (if (> (count @tiles) max-tiles)
;    (do
;      (delete-fully-connected-tiles)
;      (delete-nonconnected-tiles)
;      ;(dotimes [n 1]
;      ;  (if (= (mod @frame 1) 0)
;      ;    (delete-random-tile)))
;      ))

;   (pop-matrix)
;  (ortho)
  ;(translate [700 400 0])
  ;(draw-info)


  (reset! last-render-time
          (float (/ (- (System/nanoTime)
                       frame-start-time)
                    1000000.0)))

  (if (= (mod (frame-count) 150) 0)
   (println "last-render-time: " @last-render-time))
  ))




; _______________________________________________________________________
 
(defsketch rhombrick 
  :title "rhombrick"
  :setup setup 
  :draw draw
  :size [1900 1100]
  :renderer :opengl 
  :key-typed key-typed
  :key-pressed key-pressed
  :key-released key-released
  :mouse-moved mouse-moved)


;(sketch-start rhombrick)

;(reset! my-applet rhombrick)
