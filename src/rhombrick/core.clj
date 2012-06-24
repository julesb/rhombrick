(ns rhombrick.core
  (:use [quil.core]
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

(def mouse-position (atom [0 0]))
(def view-scale (atom 1.0))
(def model-scale (atom 50))
(def frame (atom 0))

(def num-gliders 50)

(def last-render-time (atom 0))

(def keys-down (atom #{}))

; _______________________________________________________________________
; Rendering and events 


; _______________________________________________________________________


(defn setup []
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
    (make-tiling-iteration) ; needed so init-gliders works
    (init-gliders num-gliders)
    ;(println @gliders)
    ;(init-todo)

;    (doseq [val (range 10)]
;      (osc-send client "/test" "i" (float val)))


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

(defn mouse-delta [scale-factor]
  (let [dx (- (mouse-x) (pmouse-x))
        dy (- (mouse-y) (pmouse-y))]
    (vec3-scale [dx dy 0] scale-factor)))


(defn draw []
  (let [frame-start-time (System/nanoTime)]
  ;(let [frame-start-time (millis)]
    
  (make-tiling-iteration)
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
  (swap! frame + 1)
  (background 32 32 192)
   
  ;(reset-matrix)
  ;(push-matrix)

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
            newpos (vec3-add @camera-pos (vec3-scale dir (* d 0.040)))
            cl-d (dist (@camera-lookat 0)
                       (@camera-lookat 1)
                       (@camera-lookat 2)
                     (g 0) (g 1) (g 2))
            cl-dir (vec3-normalize (vec3-sub g @camera-lookat))
            new-camera-lookat (vec3-add @camera-lookat 
                                        (vec3-scale cl-dir
                                                    (* cl-d 0.25)))]
        (reset! camera-lookat new-camera-lookat)    
        (reset! camera-pos newpos)
        (camera (newpos 0) (newpos 1) (- (newpos 2) 3)
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
      (let [md (mouse-delta 0.005)]
        (do-camera-transform @camera-pos
                             (* 1.0 (md 1))
                             (* -1.0 (md 0)))))

  )

  (perspective (radians @camera-fov) 
                 @camera-aspect-ratio
                 @camera-near-clip
                 @camera-far-clip)
  (lights)  
  ;(light-falloff 0.5 0.0 0.0) 
  (light-specular 255 0 0)
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

    (draw-gliders @frame)
    (draw-face-list)
    (draw-tiling)

    (stroke-weight 1)
    ;(draw-todo)

    (fill 192 192 192 255)
    (draw-todo-head)
    
    ;(no-fill)
    ;(stroke-weight 1)
    ;(stroke 255 255 255 255)
    ;(draw-normalized-facecodes @frame )
    (pop-matrix)

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
          (float (/ (- (System/nanoTime) frame-start-time) 1000000.0)))

  (if (= (mod @frame 150) 0)
   (println "last-render-time: " @last-render-time))
  ))



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
         (make-tiling-iteration)
         (init-gliders num-gliders))
   \R #(do 
         (init-tiler)
         (random-tileset)
         (make-tiling-iteration)
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
         (reset! camera-mode m)))
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

; _______________________________________________________________________

(defn do-movement-keys []
  (doseq [k @keys-down]
    (if (contains? key-movement-map k)
      (do
        ((key-movement-map k))))))
    

(defn key-typed []
  (let [keychar (raw-key)]
    (if (contains? key-command-map keychar)
      ((key-command-map keychar)))))


(defn key-pressed []
  (swap! keys-down conj (raw-key))
  (do-movement-keys))


(defn key-released []
  (swap! keys-down disj (raw-key))
  (do-movement-keys))



; _______________________________________________________________________

(defn mouse-moved []
  (let [x (mouse-x) y (mouse-y)]
    (reset! (state :mouse-position) [x y])))

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
