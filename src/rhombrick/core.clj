(ns rhombrick.core
  (:use [quil.core]
        [quil.applet]
        ;[rhombrick.facecode]
        ;[rhombrick.staticgeometry :as geom]
        [rhombrick.tiling :as tiling]
        [rhombrick.tiling-render]
        [rhombrick.bezierbox :as bbox]
        [rhombrick.game :as game]
        [rhombrick.vector]
        [rhombrick.glider]
        [rhombrick.camera]
        [rhombrick.button]
        [rhombrick.editor :as editor]
        [rhombrick.console :as console]
        [clojure.math.combinatorics]
        [overtone.osc]
        )
  (:import java.awt.event.KeyEvent))

(import processing.core.PImage)
(import java.awt.Robot)
(def robot (new java.awt.Robot))

(def mouse-position (atom [0 0]))
(def view-scale (atom 1.0))
;(def model-scale (atom 50))
;(def frame (atom 0))

;(def num-gliders 50)

(def last-render-time (atom 0))

(def keys-down (atom #{}))

;(def mousewarp-pos (atom [716 356])) ; 1440x800
(def mousewarp-pos (atom [946 506])) ; 1900x1100
(def last-mouse-delta (atom [0 0]))

(def my-applet (atom nil))
(def frame (atom nil))

(def draw-facelist? (atom false))
(def draw-editor? (atom false))
(def draw-gliders? (atom false))
(def draw-bezier-box-lines? (atom false))
(def draw-bezier-box-faces? (atom true))
(def draw-tilecode-lines? (atom false))
(def draw-console? (atom false))
(def draw-graphs? (atom false))
(def tiler-auto-seed? (atom false))

(def boundary-mode-idx (atom 0))
(def boundary-modes [:only-empty :type-change :all :none])
(def current-boundary-mode (atom (boundary-modes @boundary-mode-idx)))

(def game-mode? (atom true))

(def ^:dynamic editor-font)
(def ^:dynamic console-font)

; _______________________________________________________________________


;(defn get-location-on-screen []
;  (let [p (.getLocationOnScreen @my-applet)
;        x (.x p)
;        y (.y p)]
;    ;(reset! mousewarp-pos [(- (/ (width) 2) (* x 2))
;    ;                       (- (/ (height) 2) (* y 2)) ])
;    [x y]))

; _______________________________________________________________________
; Rendering and events 


; _______________________________________________________________________


(defn setup []
    ;(reset! rhomb-tex (load-image "cave_texture_01-512x512.png"))
    ;(reset! rhomb-tex (load-image "testpattern4po6.png"))
    (reset! rhomb-tex (load-image "gradient.jpg"))
    ;(reset! rhomb-tex (load-image "map-15Bsubset.jpg"))

    ;(println "texture:" @rhomb-tex)
    (texture-mode :normalized)
      
    ;(println "screen pos" (get-location-on-screen))
    ;(println "screen size:" (width) (height))
    ;(println "frame:" (.getLocation (.frame @my-applet)))
    ;(.mouseMove robot 0 0)
    ;(.mouseMove robot (/ (width) 2) (/ (height) 2))
    
    (smooth)
    (frame-rate 60)
    (update-camera)
    (println "setting font")
    ;(text-font (load-font "FreeMono-16.vlw"))
    ;(text-font (load-font "ScalaSans-Caps-32.vlw"))
    (def console-font (load-font "FreeMono-16.vlw"))
    (def editor-font (load-font "AmericanTypewriter-24.vlw"))
    (text-font (load-font "AmericanTypewriter-24.vlw"))
    (set-state! :mouse-position (atom [0 0]))

    (println "initialising tiler")
    (editor/init-editor)
    (if @game-mode?
      (reset! camera-mode 3)
      (start-game (editor/get-tileset-expanded))
      ;(start-tiler (editor/get-tileset-as-set) false)
      )
    ;(init-gliders num-gliders)
    ;(println @gliders

;    (doseq [val (range 10)]
;      (osc-send client "/test" "i" (float val)))

    ;(reset! mousewarp-pos [(mouse-x) (mouse-y)])
    ;(println "mouse:" @mousewarp-pos)

    (Thread/setDefaultUncaughtExceptionHandler
      (reify Thread$UncaughtExceptionHandler
        (uncaughtException [this thread throwable]
          ;; do something with the exception here.. log it, for example.
          (do
            (println "UNCAUGHT EXCEPTION:" this thread throwable)
            (println "STACK TRACE:")
            (clojure.stacktrace/print-stack-trace throwable)
            (println "CAUSE TRACE:")
            (clojure.stacktrace/print-cause-trace throwable)
            )
     )))

    (println "setup done")
    )

; _______________________________________________________________________

(defn draw-info [x y]
  (when (zero? @last-iteration-time)
    (reset! last-iteration-time 1))
  (text-font console-font)
  (let [line-space 22
        lines [(str "game mode:" @game-mode?)
               (str "candidates:" @game/candidates)
               (str "run state: " @tiler-run-state)
               (str "iters: " @tiler-iterations)
               (str "tiles: " (count @tiles) "/" @max-tiles) 
               (str "ips: " (int (/ 1000 @last-iteration-time)))
               ;(str "empty: " (count @empty-positions)) 
               (str "dead: " (count @dead-loci))
               (str "radius: " @assemblage-max-radius)
               ;(str "-------------")
               (str "bbox detail: " @bbox/bezier-box-resolution)
               (str "scale: " @model-scale)
               (str "fps: " (int (current-frame-rate)))
               (str "tileset:" (get-tileset))
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
         (start-tiler (editor/get-tileset-as-set) true)
         (init-gliders num-gliders)
         )
   \R #(do 
         (editor/set-tileset (get-random-tileset))
         (start-tiler (editor/get-tileset-as-set) false)
         (println "random tileset:" (editor/get-tileset-as-set))
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
   \C #(do
         ;(swap! draw-console? not)
          (let [outercode (get-outer-facecode2 (get-neighbourhood @tiles @game/selected-pos))
                ;new-code (make-random-tilecode-to-fit outercode)
                new-code (make-minimal-tilecode-to-fit outercode)
                ]
            (editor/add-to-tileset new-code)
            (make-tile! @selected-pos new-code)
            (osc-send client "/rhombrick.game" "place-tile" @game/selected-candidate-idx)
            (game/game-step (editor/get-tileset-expanded))
         ))
    \F #(do
         (swap! draw-facelist? not)
         (when @draw-facelist?
           (build-face-list))
         (println "draw facelist? " @draw-facelist?))
    \g #(do
          (swap! draw-gliders? not)
          (if draw-gliders?
            (init-gliders num-gliders)))
    \G #(do
          (swap! draw-graphs? not))
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
    \p #(do
          (cond 
            (= @tiler-run-state :running) (reset! tiler-run-state :paused)
            (= @tiler-run-state :paused)  (reset! tiler-run-state :running)))
    \( #(do
          (swap! assemblage-max-radius dec))
    \) #(do
          (swap! assemblage-max-radius inc))

    \< #(do
          (editor/load-prev-library-tileset)
          (start-tiler (editor/get-tileset-as-set) false))
    \> #(do
          (editor/load-next-library-tileset)
          (start-tiler (editor/get-tileset-as-set) false))
    \S #(do
          (editor/save-current-tileset-to-library))
    ;\n #(do
    ;      (swap! symmetry-display-index dec))
    ;\m #(do
    ;      (swap! symmetry-display-index inc))
    \# #(do
          (save-frame))
    \b #(do
          (reset! boundary-mode-idx (mod (inc @boundary-mode-idx)
                                         (count boundary-modes)))
          (reset! current-boundary-mode (boundary-modes @boundary-mode-idx))
          (println "boundary mode: " @current-boundary-mode))
    \l #(do
          (swap! draw-bezier-box-lines? not))
    \L #(do
          (swap! draw-tilecode-lines? not))
    \f #(do
          (swap! draw-bezier-box-faces? not))
    \_ #(do
          (when (> @bezier-box-resolution 1)
            (swap! bezier-box-resolution dec)
            (bezier-box-cache-reset)))
    \+ #(do
          (when (< @bezier-box-resolution 32)
            (swap! bezier-box-resolution inc)
            (bezier-box-cache-reset)))
    \; #(do
          (swap! bezier-box-control-bias (fn [n] (- n 0.01)))
          (bezier-box-cache-reset)
          (println "bezierbox control bias:" @bezier-box-control-bias))
    \' #(do
          (swap! bezier-box-control-bias (fn [n] (+ n 0.01)))
          (bezier-box-cache-reset)
          (println "bezierbox control bias:" @bezier-box-control-bias))
    \* #(do
          (swap! bezier-box-line-weight inc))
    \& #(do
          (swap! bezier-box-line-weight dec))
    \M #(do
          (swap! bezier-box-smooth-shading? not))
    \Z #(do
          (swap! game-mode? not))
    \z #(do
          (game/start-game (editor/get-tileset-expanded)))
    \u #(do
          (osc-send client "/rhombrick.game" "place-tile" @game/selected-candidate-idx)
          (game/game-step (editor/get-tileset-expanded))
          )
    \U #(do
          (reset! game/selected-candidate-idx (int (rand (count @game/candidates))))
          (game/game-step (editor/get-tileset-expanded))
        )
    \j #(do
          (osc-send client "/rhombrick.game" "backtrack" @game/selected-candidate-idx)
          (game/do-backtrack)
          (game/update-game-state (editor/get-tileset-expanded))
          )
    \h #(do
          (osc-send client "/rhombrick.game" "change-candidate" @game/selected-candidate-idx)
          (game/prev-candidate)
          (update-neighbour-candidates @tiling/tiles (editor/get-tileset-expanded)))
    \k #(do
          (osc-send client "/rhombrick.game" "change-candidate" @game/selected-candidate-idx)
          (game/next-candidate)
          (update-neighbour-candidates @tiling/tiles (editor/get-tileset-expanded)))
    \D #(do
          (osc-send client "/rhombrick.game" "destroy-neighbourhood" @game/selected-candidate-idx)
          (game/destroy-neighbourhood)
          (game/update-game-state (editor/get-tileset-expanded))
          )
       })

(def key-editor-map
  {
    KeyEvent/VK_UP    #(do (editor/level-down))
    KeyEvent/VK_DOWN  #(do (editor/level-up))
    KeyEvent/VK_LEFT  #(do (editor/move-left))
    KeyEvent/VK_RIGHT #(do (editor/move-right))
    \j #(do (editor/level-up))
    \k #(do (editor/level-down))
    \h #(do (editor/move-left))
    \l #(do (editor/move-right))
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
  (let [the-raw-key (raw-key)
        the-key-code (key-code)
        coded? (= processing.core.PConstants/CODED (int the-raw-key))
        the-key-pressed (if coded? the-key-code raw-key) ]
    (if (and coded? (contains? key-editor-map the-key-pressed))
      ((key-editor-map the-key-pressed))
      (swap! keys-down conj the-raw-key))))


(defn key-released []
  (swap! keys-down disj (raw-key)))

; _______________________________________________________________________


(defn mouse-moved []
  (let [x (mouse-x) y (mouse-y)
        delta [(- (mouse-x) (@mousewarp-pos 0))
               (- (mouse-y) (@mousewarp-pos 1)) 0]]
    (when (not (> (editor/get-level) 0))
      (reset! last-mouse-delta (vec3-scale delta 0.01))
      (reset! (state :mouse-position) [x y]))
    (when (> (editor/get-level) 0)
      (update-ui-state :mouse-x (mouse-x))
      (update-ui-state :mouse-y (mouse-y)))))


(defn mouse-pressed []
  ;(println "mouse-pressed")
  (update-ui-state :mouse-down true))


(defn mouse-released []
  ;(println "mouse-released")
  (update-ui-state :mouse-down false))
  
; _______________________________________________________________________


(defn draw-horizon []
  (stroke 0 255 0 128)
  (stroke-weight 1)
  (ellipse 0 0 200 200))

(defn draw-assemblage-radius []
  (let [rad (* @assemblage-max-radius 2)]
    (stroke 255 0 0 128)
    (stroke-weight 1)
    (ellipse 0 0 rad rad)))



(defn auto-seed-tiler []
  (when (or (and (> @tiler-iterations 100)
                 (< (count @tiles) 5))
            (= @tiler-run-state :halted))
      (editor/set-tileset (get-random-tileset))
      (init-tiler (editor/get-tileset-as-set))
      (println "random tileset:" (editor/get-tileset-as-set)) 
      ;(make-backtracking-tiling-iteration2 @tiles (editor/get-tileset-as-set))
      (init-gliders num-gliders)))


(defn draw []
  ;(get-location-on-screen)
  (let [frame-start-time (System/nanoTime)]

  ;(when (< (editor/get-level) 2)
    (do-movement-keys)
  ;  )

;  (when @tiler-auto-seed?
;    (auto-seed-tiler))
   
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
            ;(if (not @draw-editor?)
            (when (not (> (editor/get-level) 0))
              (do
                (reset! last-mouse-delta (mouse-delta 0.0001))
                (.mouseMove robot (/ (width) 2) (/ (height) 2))))))
    (= @camera-mode 3)
    ; game mode camera
      (do
        (let [g (vec3-scale @selected-pos @model-scale)
              ndir (vec3-normalize (vec3-sub @assemblage-center @selected-pos))
              target-pos (vec3-add g (vec3-scale ndir (* @model-scale -5.0 )))
              dir-to-target (vec3-normalize (vec3-sub target-pos @camera-pos))
              dist-to-target (dist (@camera-pos 0) (@camera-pos 1) (@camera-pos 2)
                                   (target-pos 0) (target-pos 1) (target-pos 2))
              newpos (vec3-add @camera-pos (vec3-scale dir-to-target (* dist-to-target 0.250)))
              cl-d (dist (@camera-lookat 0)
                         (@camera-lookat 1)
                         (@camera-lookat 2)
                       (g 0) (g 1) (g 2))
              cl-dir (vec3-normalize (vec3-sub g @camera-lookat))
              new-camera-lookat (vec3-add @camera-lookat 
                                          (vec3-scale cl-dir
                                                      (* cl-d 0.25))) ]
          (reset! camera-lookat new-camera-lookat)    
          (reset! camera-pos newpos)
          (camera (newpos 0) (newpos 1) (+ (newpos 2) 0)
                  (new-camera-lookat 0)
                  (new-camera-lookat 1)
                  (new-camera-lookat 2)
                  0 0 -1)))
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
    
    (lights)
    (when @draw-gliders?
      (draw-gliders (frame-count))
    )

;    (push-matrix)
;    (rotate-x (/ (frame-count) 200.1))
;    (rotate-y (/ (frame-count) 180.73))
;    (let [n (vec3-normalize [-0.5 0.5 -0.5])]
;      (directional-light 64 64 255 (n 0) (n 1) (n 2)))
;    (let [n (vec3-normalize [-0.5 0.5 0.5])]
;      (directional-light 64 128 64 (n 0) (n 1) (n 2)))
;    (let [n (vec3-normalize [-0.5 0.5 0.0])]
;      (directional-light 255 64 64 (n 0) (n 1) (n 2)))
;    (pop-matrix)
   
    (push-matrix)
;    (rotate-z (/ (frame-count) 40.1))
;    (rotate-x (/ (frame-count) 41.231))
;    (rotate-y (/ (frame-count) 38.73))

    (let [r (/ @assemblage-max-radius 2)
          mr (/ (- @assemblage-max-radius) 2)]
      (fill 0 255 0)
      (with-translation [mr  r  r] (box 0.1))
      (fill 255 0 0)
      (with-translation [ r mr  r] (box 0.1))
      (fill 0 0 255)
      (with-translation [ r  r mr] (box 0.1))

      (light-falloff 1.1 0.0 0.0)

      ;(spot-light 0 255 0 mr  r  r   1  -1  -1 (/ Math/PI 2) 2)
      ;(spot-light 255 0 0  r mr  r  -1   1  -1 (/ Math/PI 2) 2)
      ;(spot-light 0 0 255  r  r mr  -1  -1   1 (/ Math/PI 2) 2))
      (spot-light 128 255 128 mr  r  r   1  -1  -1 (/ Math/PI 2) 2)
      (spot-light 255 128 128  r mr  r  -1   1  -1 (/ Math/PI 2) 2)
      (spot-light 128 128 255  r  r mr  -1  -1   1 (/ Math/PI 2) 2))
    (pop-matrix)

    ;(light-falloff 1.0 0.2 0.0)
    ;(ambient-light 64 64 64)

    ;(hint :disable-depth-test) 
    ;(draw-tiling)
    ; (hint :enable-depth-test)
    (no-fill)
    (draw-horizon)
    (draw-assemblage-radius)

    ;(when @draw-facelist?
    ;  (draw-face-list))
      ;(draw-face-list-textured))

    (update-selected-pos-screen)
    (update-neighbour-candidates-screen)

    (draw-tiling true ;(not= @current-boundary-mode :none)
                 @draw-tilecode-lines?
                 @draw-bezier-box-faces?
                 @draw-bezier-box-lines?
                 @current-boundary-mode)
    
    (draw-assemblage-center)

    (when @draw-facelist?
      (draw-face-list))

    (when @game-mode?
      (game/render))

    (when (seq (get-empty-positions @tiles @assemblage-max-radius))
      (draw-empty @tiles))
    

    ;(lights)
    (when @draw-gliders?
      (let [selected-tile ((get-glider 1) :current-tile)]
        ;(draw-neighbours selected-tile)
        ;(draw-curve-boundary-points selected-tile)
        (draw-selected-tile selected-tile)
        ))
    (pop-matrix)
  )

  (pop-matrix)
  
  ; 2d hud stuff
  ;(hint :disable-depth-test)
  ;(camera)
  ;(ortho)
  (hint :disable-depth-test)
  (draw-info 10 (- (height) 250))
  (camera)

  (when @draw-console?
    (text-font console-font)
    (console/draw-buffer 230 100))

  (when (> (editor/get-level) 0)
    (text-font editor-font)
    (draw-tileset-editor [20 20] (editor/get-tileset) 64)
    ;(doseq [i (range (count @face-id-text))]
    ;  (let [f (@face-id-text i)
    ;        t (str (f 0))
    ;        x 20 ;(* 100 (+ 20 ((f 1) 0)))
    ;        y 20 ] ;(* 100 (+ 20 ((f 1) 1)))]
    ;  (fill 255 255 255 255)
    ;  (text t x y)))
    )

  (when @game-mode?
    (game/render-2d))

    ; bottom of screen
    ;(draw-tileset-editor [20 (- (height) 180)] @current-tileset 140))
    ;(draw-tileset-editor [1285 20] @current-tileset 140))

  (when @draw-graphs?
    (draw-graphs [20 100]))

  (hint :enable-depth-test)

  (reset! last-render-time
          (float (/ (- (System/nanoTime)
                       frame-start-time)
                    1000000.0)))

  ))




; _______________________________________________________________________


(defn -main [& args]
  (defsketch rhombrick 
    :title "rhombrick"
    :setup setup 
    :draw draw
    :size [1900 1100]
    ;:size [1440 800]
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
