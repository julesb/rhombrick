(ns rhombrick.core
  (:use [quil.core :exclude [target-frame-rate]]
        [quil.applet]
        [rhombrick.tiling :as tiling]
        [rhombrick.tilecode :as tc]
        [rhombrick.tiling-render]
        [rhombrick.bezierbox :as bbox]
        [rhombrick.tile-shape-2d]
        [rhombrick.staticgeometry]
        [rhombrick.game :as game]
        [rhombrick.vector]
        [rhombrick.glider]
        [rhombrick.camera]
        [rhombrick.button]
        [rhombrick.editor :as editor]
        [rhombrick.console :as console]
        [rhombrick.marching-cubes]
        [clojure.math.combinatorics]
        [overtone.osc]
        )
  (:import java.awt.event.KeyEvent))

(import processing.core.PImage)
(import processing.core.PShape)
;(import processing.core.PShader)

(import java.awt.Robot)
(def robot (new java.awt.Robot))

(def mouse-position (atom [0 0]))
;(def num-gliders 50)

(def last-render-time (atom 0))

(def keys-down (atom #{}))

;(def mousewarp-pos (atom [716 356])) ; 1440x800
(def mousewarp-pos (atom [946 506])) ; 1900x1100
(def last-mouse-delta (atom [0 0]))

(def my-applet (atom nil))
(def frame (atom nil))

(def fullscreen? (atom false))
(def draw-facelist? (atom false))
(def draw-editor? (atom false))
(def draw-gliders? (atom false))
(def draw-bezier-box-lines? (atom false))
(def draw-bezier-box-faces? (atom true))
(def draw-tilecode-lines? (atom false))
(def draw-tilecode-blobs? (atom false))
(def draw-console? (atom false))
(def draw-graphs? (atom false))
(def draw-empty? (atom true))
(def draw-info? (atom true))
(def tiler-auto-seed? (atom false))

(def boundary-mode-idx (atom 1))
(def boundary-modes [:only-empty :type-change :all :none])
(def current-boundary-mode (atom (boundary-modes @boundary-mode-idx)))

(def game-mode? (atom false))
(def auto-seed? (atom false))

(def ^:dynamic editor-font)
(def ^:dynamic console-font)

(def to-verts-screen (atom []))
(def anchor-verts-screen (atom []))
;(def test-surface (atom {}))
(def rendering? (atom true))

(def edge-shader (atom nil))

(def phi-palette-offset (atom 0))

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
    ;(reset! sky-tex (load-image "testpattern4po6.png"))
    (reset! sky-tex (load-image "Sky02.jpg"))
    ;(reset! skybox-tex (load-image "stormydays_large.jpg"))
    ;(reset! skybox-tex (load-image "grimmnight_large.jpg"))
    ;(reset! skybox-tex (load-image "interstellar_large.jpg"))
    ;(reset! skybox-tex (load-image "Above_The_Sea.jpg"))
    (reset! skybox-tex (load-image "miramar_large.jpg"))
    ;(reset! skybox-tex (load-image "cube_filament.jpg"))
    ;(reset! trunc-oct-tex (load-image "truncated_octahedron.obj"))
    ;(reset! rhomb-tex (load-image "cave_texture_01-512x512.png"))
    ;(reset! rhomb-tex (load-image "testpattern4po6.png"))
    (reset! bbox-tex (load-image "cave_texture_01-512x512.png"))
    ;(reset! bbox-tex (load-image "seamless_texture__circuit_by_ark4n-d2palho.jpg"))
    ;(reset! bbox-tex (load-image "seamless_moss_rocks.jpg"))
    ;(reset! bbox-tex (load-image "seamless-black-wall-texture-decorating-inspiration-1.jpg"))
    ;(reset! rhomb-tex (load-image "gradient.jpg"))
    ;(reset! rhomb-tex (load-image "map-15Bsubset.jpg"))
    ;(reset! bound-tex (load-image "seamless_metal_texture_by_hhh316-d30x412.jpg"))
    ;(reset! bound-tex (load-image "rhombrick-boundary-uv-edgehalo.png"))
    (reset! bound-tex (load-image "rhombrick-boundary-uv-scifi.png"))
    ;(reset! particle-texture (load-image "NeGeo-particle.png"))
    (reset! particle-texture (load-image "pump_flare_06.png"))
    ;(println "texture:" @rhomb-tex)
    (texture-mode :normal)
    (texture-wrap :repeat)

    ;(println "screen pos" (get-location-on-screen))
    ;(println "screen size:" (width) (height))
    ;(println "frame:" (.getLocation (.frame @my-applet)))
    ;(.mouseMove robot 0 0)
    ;(.mouseMove robot (/ (width) 2) (/ (height) 2))
    ;(smooth)
    ;(def edge-shader (load-shader "data/edges.glsl"))
    (reset! edge-shader (load-shader "data/test.frag"))
    (frame-rate 60)
    (update-camera)
    (println "setting font")
    ;(text-font (load-font "FreeMono-16.vlw"))
    ;(text-font (load-font "ScalaSans-Caps-32.vlw"))
    (def console-font (load-font "data/FreeMono-16.vlw"))
    (def editor-font (load-font "data/AmericanTypewriter-24.vlw"))
    (text-font (load-font "data/AmericanTypewriter-24.vlw"))
    (set-state! :mouse-position (atom [0 0]))

    (println "initialising tiler")
    (editor/init-editor)
    (hint :enable-stroke-perspective)
;    (if @game-mode?
;      (reset! camera-mode 3)
;      (start-game (editor/get-tileset-expanded))
;      ;(start-tiler (editor/get-tileset-as-set) false)
;      )

  ;(init-gliders num-gliders)
    ;(println @gliders

;    (doseq [val (range 10)]
;      (osc-send client "/test" "i" (float val)))

    (reset! mousewarp-pos [(/ (width) 2) (/ (height) 2)])
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
               (str "topology:" (@current-topology :id))
               ;(str "candidates:" @game/candidates)
               (str "run state:" (@tiler-state :run-status))
               (str "solved:" (@tiler-state :solved))
               (str "iters: " (@tiler-state :iters) "/" (get-in @tiler-state [:params :max-iters]))
               (str "tiles: " (count (@tiler-state :tiles)) "/" ((@tiler-state :params) :max-tiles))
               (str "ips: " (int (/ 1000 @last-iteration-time)))
               (str "tiles/iter: " (format "%.3f" (double (/ (count (@tiler-state :tiles))
                                                             (inc (@tiler-state :iters) )))))
               (str "empty:" (count (@tiler-state :empty)))
               (str "dead: " (count (@tiler-state :dead)))
               (str "radius: " ((@tiler-state :params) :max-radius))
               ;(str "-------------")
               (str "bbox detail: " @bbox/bezier-box-resolution)
               (str "cam:" (vec3-format @camera-pos))
               (str "scale: " @model-scale)
               (str "fps: " (int (current-frame-rate)))
               (str "tileset:" (get-tileset))
               (str "seed:" "\"" (get-in @tiler-state [:params :seed]) "\"")
               ]]
    (let [rectw (+ 20 (max 300 (text-width (str "tileset:" (get-tileset)))))]
      (fill 0 0 0 192)
      (rect (- x 10.0) (- y 20.0) rectw 400))
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
         ;(println "tiling bounding box: " (get-assemblage-extents @tiler-state))
         (println "model-scale: " @model-scale)
         )
   \. #(do
        (swap! model-scale - 1.0)
        ;(println "tiling bounding box: " (get-assemblage-extents @tiler-state))
        (println "model-scale: " @model-scale))
   ;\r #(make-cubic-tiling 10 10 10)
   \r #(do
         (println "restart tileset:" (editor/get-tileset-as-set))
         (shape-2d-cache-reset)
         (start-tiler (editor/get-tileset-as-set) false)
         (init-tileset-colors (editor/get-tileset-as-set) @phi-palette-offset)
         ;(init-tileset-colors (get-in @tiler-state [:params :tileset]) @phi-palette-offset)
         (init-gliders num-gliders)
         )
   \R #(do
         (editor/set-tileset (vec (distinct (normalize-tileset (get-random-tileset-1)))))
         (start-tiler (editor/get-tileset-as-set) false)
         (init-tileset-colors (get-in @tiler-state [:params :tileset]) @phi-palette-offset)
         (println "random tileset:" (editor/get-tileset-as-set))
         (init-gliders num-gliders)
         )
   \A #(do
         (swap! auto-seed? not) 
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
;   \C #(do
;         ;(swap! draw-console? not)
;          (let [outercode (get-outer-facecode2 (get-neighbourhood @tiles @game/selected-pos))
;                ;new-code (make-random-tilecode-to-fit outercode)
;                new-code (make-minimal-tilecode-to-fit outercode)
;                ]
;            (editor/add-to-tileset new-code)
;            (make-tile! @selected-pos new-code)
;            (osc-send client "/rhombrick.game" "place-tile" @game/selected-candidate-idx)
;            (game/game-step (editor/get-tileset-expanded))
;         ))
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
         (cancel-tiler-thread)
         (Thread/sleep 100)
         (reset! current-topology (topologies (next-topology (@current-topology :id))))
         (editor/set-tileset (get-random-tileset-1))
         (editor/set-tileset-topo-id (@current-topology :id))
         (start-tiler (editor/get-tileset-as-set) false)
         (init-tileset-colors (editor/get-tileset-as-set) @phi-palette-offset)
          (init-gliders num-gliders)
         ;(swap! max-tiles inc)
         ;(println "max tiles:" @max-tiles)
        )
    \t #(do

         ;(swap! max-tiles dec)
         ;(println "max tiles:" @max-tiles)
        )
    \[ #(do
          ;(swap! adhd - 0.1)
          ;(println "adhd:" @adhd "auti:" @autism)
          )
    \] #(do
          ;(swap! adhd + 0.1)
          ;(println "adhd:" @adhd "auti:" @autism)
          )
    \{ #(do
          ;(swap! autism - 0.1)
          ;(println "adhd:" @adhd "auti:" @autism)
          )
    \} #(do
          ;(swap! autism + 0.1)
          ;(println "adhd:" @adhd "auti:" @autism)
          )
    \p #(do
          (cancel-tiler-thread)
          ;(cond
          ;  (= @tiler-run-state :running) (reset! tiler-run-state :paused)
          ;  (= @tiler-run-state :paused)  (reset! tiler-run-state :running))
          )
    \P #(do
          (if @rendering?
            (no-loop)
            (start-loop))
          (swap! rendering? not)
          )

    \( #(do
          (reset! tiler-state
                  (assoc @tiler-state :params
                                      (update-in (@tiler-state :params) [:max-radius] dec )))
          )
    \) #(do
          (reset! tiler-state
                  (assoc @tiler-state :params
                                      (update-in (@tiler-state :params) [:max-radius] inc )))
          )

    \< #(do
          (cancel-tiler-thread)
          (Thread/sleep 100)
          (editor/load-prev-library-tileset)
          (start-tiler (editor/get-tileset-as-set) false)
          (init-tileset-colors (get-in @tiler-state [:params :tileset]) @phi-palette-offset)
          (init-gliders num-gliders))
    \> #(do
          (cancel-tiler-thread)
          (Thread/sleep 100)
          (editor/load-next-library-tileset)
          (start-tiler (editor/get-tileset-as-set) false)
          (init-tileset-colors (get-in @tiler-state [:params :tileset]) @phi-palette-offset)
          (init-gliders num-gliders))
    \S #(do
          (editor/save-current-tileset-to-library))
    \n #(do
           (swap! fullscreen? not
                  )
          )
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
    \e #(do
          (swap! draw-empty? not))
    \i #(do
          (swap! draw-info? not))
    \_ #(do
          (when (> @bezier-box-resolution 1)
            (swap! bezier-box-resolution dec)
            (shape-2d-cache-reset)
            (bezier-box-cache-reset)))
    \+ #(do
          (when (< @bezier-box-resolution 32)
            (swap! bezier-box-resolution inc)
            (shape-2d-cache-reset)
            (bezier-box-cache-reset)
            ))
    \; #(do
          (swap! bezier-box-control-bias (fn [n] (- n 0.01)))
          (bezier-box-cache-reset)
          (shape-2d-cache-reset)
          (println "bezierbox control bias:" @bezier-box-control-bias))
    \' #(do
          (swap! bezier-box-control-bias (fn [n] (+ n 0.01)))
          (bezier-box-cache-reset)
          (shape-2d-cache-reset)
          (println "bezierbox control bias:" @bezier-box-control-bias))
    \* #(do
          (swap! bezier-box-line-weight (fn [w] (+ w 0.001))))
    \& #(do
          (swap! bezier-box-line-weight (fn [w] (- w 0.001))))
    \M #(do
          (swap! bezier-box-smooth-shading? not))
    \y #(do
          (make-tileset-meshes @tiler-state
                                0.0
                               ;512.0
                               ;(get-tileset-expanded)
                               ;(vec (distinct (vals (@tiler-state :tiles))))
                                32 32 32 
                               )


          ;(if (contains? (@tiler-state :tiles) [0 0 0])
          ;  (do
          ;    (run-surface-thread @tiler-state)
          ;    ;  (reset! test-surface (make-tilecode-surface
          ;    ;                            0.125
          ;    ;                            ((@tiler-state :tiles) [0 0 0])
          ;    ;                            32 32 32))
          ;    )
            ;(println "make-surface:" (count (@test-surface :tris)) "tris"))
          )
    \B #(do
          (swap! draw-tilecode-blobs? not)
          (println "draw-tilecode-blobs:" @draw-tilecode-blobs?)
          )
    \Z #(do
          (swap! game-mode? not))
;    \z #(do
;          (game/start-game (editor/get-tileset-expanded)))
    \u #(do
          (osc-send client "/rhombrick.game" "place-tile" @game/selected-candidate-idx)
          (game/game-step @tiler-state)
          )
;    \U #(do
;          (reset! game/selected-candidate-idx (int (rand (count @game/candidates))))
;          (game/game-step (editor/get-tileset-expanded))
;        )
    \j #(do
          (osc-send client "/rhombrick.game" "backtrack" @game/selected-candidate-idx)
          (game/do-backtrack)
          (game/update-game-state @tiler-state)
          )
    \h #(do
          (osc-send client "/rhombrick.game" "change-candidate" @game/selected-candidate-idx)
          (game/prev-candidate)
          (update-neighbour-candidates  @tiler-state))
    \k #(do
          (osc-send client "/rhombrick.game" "change-candidate" @game/selected-candidate-idx)
          (game/next-candidate)
          (update-neighbour-candidates  @tiler-state))
    \% #(do
          (swap! phi-palette-offset - 0.01)
          (init-tileset-colors (get-in @tiler-state [:params :tileset]) @phi-palette-offset)
          )
    \^ (fn [] (do
          (swap! phi-palette-offset + 0.01)
          (init-tileset-colors (get-in @tiler-state [:params :tileset]) @phi-palette-offset)
          ))
    \! #(do
          (reset! skybox-tex-idx (mod (inc @skybox-tex-idx) (count skybox-textures)))
          (reset! skybox-tex (load-image (skybox-textures @skybox-tex-idx)))
          )
    \@ #(do
          (reset! bbox-tex-idx (mod (inc @bbox-tex-idx) (count bbox-textures)))
          (reset! bbox-tex (load-image (bbox-textures @bbox-tex-idx)))
          )
    \` #(do
          (reset-shader)
          (reset! edge-shader (load-shader "data/test.frag"))          
          )
;    \D #(do
;          (osc-send client "/rhombrick.game" "destroy-neighbourhood" @game/selected-candidate-idx)
;          (game/destroy-neighbourhood)
;          (game/update-game-state (editor/get-tileset-expanded))
;          )
  
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
      (reset! last-mouse-delta (vec3-scale delta 0.001))
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
  (no-fill)
  (stroke 0 255 0 128)
  (stroke-weight 0.05)
  (ellipse 0 0 100 100))

(defn draw-assemblage-radius []
  (let [rad (* ((@tiler-state :params) :max-radius) 2)]
    (stroke 255 0 0 128)
    (stroke-weight 0.025)
    (ellipse 0 0 rad rad)))



;(defn auto-seed-tiler []
;  (when (or (and (> @tiler-iterations 100)
;                 (< (count @tiles) 5))
;            (= @tiler-run-state :halted))
;      (editor/set-tileset (get-random-tileset))
;      (init-tiler (editor/get-tileset-as-set))
;      (println "random tileset:" (editor/get-tileset-as-set))
;      ;(make-backtracking-tiling-iteration2 @tiles (editor/get-tileset-as-set))
;      (init-gliders num-gliders)))


(defn draw-surface [surf]
  (push-matrix)
  ;(scale (/ 1.0 (topo-coord-scales (@current-topology :id))))
  ;(scale (topo-coord-scales (@current-topology :id)))
  (scale (@current-topology :aabb-radius))
  (begin-shape :triangles)
  (doseq [[i v n] (map vector (range (count (surf :tris)))
                              (surf :tris)
                              (surf :norms)) ]
    (normal (n 0) (n 1) (n 2))
    (vertex (v 0) (v 1) (v 2)))
  (end-shape)
  (pop-matrix)
  )


(defn draw-blobs [ts]
  ;(stroke-weight 1)
  ;(stroke 96 96 96)
  (no-stroke)
  ;(no-fill)
  (doseq [[pos code] (ts :tiles)]
    (if (contains? @tileset-meshes code)
      (do
        (apply fill (get-tile-color code))
        ;(apply stroke (get-tile-color code))
        (with-translation pos
          (draw-surface (@tileset-meshes code)))
  ))))





(defn draw []
  ;(get-location-on-screen)
  (let [frame-start-time (System/nanoTime)]

  ;(when (< (editor/get-level) 2)
    (do-movement-keys)
  ;  )

    ;(blend-mode :blend)
;  (when @tiler-auto-seed?
;    (auto-seed-tiler))
; auto seed mode
  (when @auto-seed?
    (when (or
            (@tiler-state :solved?)
            (not= (@tiler-state :run-status) :runnable)
            (and (> (@tiler-state :iters) 50)
               (< (count (@tiler-state :tiles)) 2))
            (and (> (@tiler-state :iters) 1000)
               (< (count (@tiler-state :tiles)) 75))
            (and (> (@tiler-state :iters) 2000)
               (< (count (@tiler-state :tiles)) 100))
            
            ) ; early bailout / reset
       (editor/set-tileset (vec (distinct (normalize-tileset (get-random-tileset-1)))))
       (start-tiler (editor/get-tileset-as-set) false)
       (init-tileset-colors (get-in @tiler-state [:params :tileset]) @phi-palette-offset)
       (println "random tileset:" (editor/get-tileset-as-set))
       (init-gliders num-gliders)
       ))


  (when @draw-gliders?
    (update-gliders))


  (background 255 255 255 )
  ;(background 64 64 64 )
  ;(background 16 24 32)
 ; (background 0 0 0)

  (push-matrix)

  (cond
    (= @camera-mode 0)
    ; rubber band camera to glider
      (do
        ;(let [g (vec3-scale @game/selected-pos @model-scale)
        ;(let [g (vec3-scale (vec3-add (get-glider-pos @model-scale) [0.0 0.0 1.0]) 1.0)
        (let [g (vec3-add (vec3-scale (get-glider-pos 1) @model-scale) [0.0 0.0 @model-scale])
        ;(let [g (vec3-scale @assemblage-center @model-scale)
              d (dist (@camera-pos 0)
                      (@camera-pos 1)
                      (@camera-pos 2)
                      (g 0) (g 1) (g 2))
              dir (vec3-normalize (vec3-sub g @camera-pos))
              newpos (vec3-add @camera-pos (vec3-scale dir (* d 0.01)))
              cl-d (dist (@camera-lookat 0)
                         (@camera-lookat 1)
                         (@camera-lookat 2)
                       (g 0) (g 1) (g 2))
              cl-dir (vec3-normalize (vec3-sub g @camera-lookat))
              new-camera-lookat (vec3-add @camera-lookat
                                          (vec3-scale cl-dir
                                                      (* cl-d 0.05125)))]
          (reset! camera-lookat new-camera-lookat)
          (reset! camera-pos newpos)
          (camera (newpos 0) (newpos 1) (+ (newpos 2) 0)
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
    ; game mode 2d camera
      (do
         ;(let [p (vec3-scale @game/selected-pos @model-scale)
         (let [p (vec3-scale @assemblage-center @model-scale)
               target [(p 0) (p 1) 1000.0 ]
               ;target [(p 0) (p 1) (* 20.0 @model-scale) ]
               d (vec3-distance @camera-pos target)
               dir (vec3-normalize (vec3-sub target @camera-pos))
               newpos (vec3-add @camera-pos (vec3-scale dir (* d 0.05)))
               cl-d (vec3-distance @camera-lookat p)
               ;cl-d (dist (@camera-lookat 0)
               ;           (@camera-lookat 1)
               ;           (@camera-lookat 2)
               ;           (p 0) (p 1) (p 2))
               cl-dir (vec3-normalize (vec3-sub p @camera-lookat))
               new-camera-lookat (vec3-add @camera-lookat
                                           (vec3-scale cl-dir
                                                       (* cl-d 0.05)))
               ]
            (reset! camera-lookat new-camera-lookat)
            (reset! camera-pos newpos)
            (camera (newpos 0) (newpos 1) (+ (newpos 2) 0)
                    (new-camera-lookat 0)
                    (new-camera-lookat 1)
                    (new-camera-lookat 2)
                    1 0 0)
            ;(rotate (/ Math/PI 6.0) 0 0 1)
           )
           )
  )

  (perspective (radians @camera-fov)
                 @camera-aspect-ratio
                 @camera-near-clip
                 @camera-far-clip)
  (no-lights)
  ;(draw-skysphere @camera-pos)
  (draw-skybox @camera-pos)


  ;(ortho)
  ;(let [w (/ (width) 2)
  ;      h (/ (height) 2) ]
  ;  (ortho (- w) w (- h) h -500 500))

  (let [[mx my] @(state :mouse-position)]
    (push-matrix)
    (scale @model-scale)
    ;(rotate (/ (frame-count) 200.0) 0 0 1)

    ;(stroke 0 255 255 128)
    ;(stroke-weight 1)
    ;(no-fill)
    ;(box 10 10 10)
    (reset! to-verts-screen (into [] (map world-to-screen (@current-topology :verts))))

    (lights)
    (when @draw-gliders?
      (push-matrix)
      ;(scale (@current-topology :aabb-radius))
      (draw-gliders (frame-count))
      (pop-matrix)
    )

    ;(draw-axes)
;    (push-matrix)
;    (rotate-x (/ (frame-count) 200.1))
;   ;(rotate-y (/ (frame-count) 180.73))
;    (let [n (vec3-normalize [-0.5 0.5 -0.5])]
;      (directional-light 64 64 255 (n 0) (n 1) (n 2)))
;    (let [n (vec3-normalize [-0.5 0.5 0.5])]
;      (directional-light 64 128 64 (n 0) (n 1) (n 2)))
;    (let [n (vec3-normalize [-0.5 0.5 0.0])]
;      (directional-light 255 64 64 (n 0) (n 1) (n 2)))
;    (pop-matrix)
    ;(ambient-light 20 22 25)
    (light-specular  25 27 32)
    
    (light-falloff 0.75 0.0 0.0)
  
    (let [pos (vec3-add (vec3-scale (get-glider-pos 1) 1.0) [0 0 1])]
      ;(no-lights)
;      (with-translation pos
;        (fill 255 255 255)
;        (sphere 0.1))
      
      ;(spot-light 255 255 255 (pos 0) (pos 1) (pos 2)  0  0 -1 (/ Math/PI 2) 4)
      (point-light 255 255 255 (pos 0) (pos 1) (pos 2))
      )
    (let [dir (vec3-scale (vec3-normalize [0.5664 -0.4369 1.0]) -1.0) ]
      (directional-light 201 226 255 (dir 0) (dir 1) (dir 2)))

    (push-matrix)
;    (rotate-z (/ (frame-count) 40.1))
;    (rotate-x (/ (frame-count) 41.231))
;    (rotate-y (/ (frame-count) 38.73))
;    (no-lights)
;    (draw-skysphere @camera-pos)

    (let [max-rad ((@tiler-state :params) :max-radius)
          max-rad (+ max-rad 1.0)
          r (/ max-rad 2)
          mr (/ (- max-rad) 2)]
      (fill 0 255 0)
      (with-translation [mr  r  r] (box 0.1))
      (fill 255 0 0)
      (with-translation [ r mr  r] (box 0.1))
      (fill 0 0 255)
      (with-translation [ r  r mr] (box 0.1))

      ;(light-falloff 1.1 0.0 0.0)
      ;(spot-light 0 255 0 mr  r  r   1  -1  -1 (/ Math/PI 2) 2)
      ;(spot-light 255 0 0  r mr  r  -1   1  -1 (/ Math/PI 2) 2)
      ;(spot-light 0 0 255  r  r mr  -1  -1   1 (/ Math/PI 2) 2))
      ;(spot-light 128 255 128 mr  r  r   1  -1  -1 (/ Math/PI 2) 2)
      ;(spot-light 255 128 128  r mr  r  -1   1  -1 (/ Math/PI 2) 2)
      ;(spot-light 128 128 255  r  r mr  -1  -1   1 (/ Math/PI 2) 2)
      ;(spot-light 255 255 255 mr  r  r   1  -1  -1 (/ Math/PI 2) 2)
      ;(spot-light 255 255 255  r mr  r  -1   1  -1 (/ Math/PI 2) 2)
      ;(spot-light 255 255 255  r  r mr  -1  -1   1 (/ Math/PI 2) 2)
      )
    (pop-matrix)

    ;(light-falloff 1.0 0.2 0.0)
    ;(ambient-light 64 64 64)

    ;(hint :disable-depth-test)
    ;(draw-tiling)
    ; (hint :enable-depth-test)
    ;(no-fill)
    ;(draw-horizon)
    ;(draw-assemblage-radius)

    ;(when @draw-facelist?
    ;  (draw-face-list))
      ;(draw-face-list-textured))

;    (if (= (@current-topology :id) :hexagon)
;      (rotate (+ (/ Math/PI 3.0) (/ Math/PI 2.0))
;              0 0 1))

    ; game:
    (update-selected-pos-screen)
    (update-neighbour-candidates-screen)

    (draw-tiling @tiler-state
                 true ;(not= @current-boundary-mode :none)
                 @draw-tilecode-lines?
                 @draw-bezier-box-faces?
                 @draw-bezier-box-lines?
                 @current-boundary-mode)

    ;(draw-assemblage-center)

    (when @draw-tilecode-blobs?
      ;(draw-surface @test-surface)
      (draw-blobs @tiler-state))

    (when @draw-facelist?
      (build-face-list)
      (draw-face-list))

    (when @game-mode?
      (game/render-game @tiler-state))

    ;(fill 0 0 0 32)
    ;(stroke 140 140 140 190)
    ;(no-fill)
    ;(draw-obj (map #(rhombrick.obj-loader/get-verts (@current-topology :verts) %)
    ;               (@current-topology :faces)) [])

    ;(draw-vert-numbers (@current-topology :verts))

;    (let [sym-ang ((symmetries-flattened @debug-symmetry-idx) 0)
;          axis ((symmetries-flattened @debug-symmetry-idx) 1) ]
;      (push-matrix)
;      (rotate (radians sym-ang) (axis 0) (axis 1) (axis 2))
;      (draw-face-idx-numbers [0 0 0] true)
;      (pop-matrix))

    ;(draw-face-idx-numbers [0 0 0] false)

;    (reset! anchor-verts-screen
;            (vec (map (fn [vs] (vec (map world-to-screen vs)))
;                      (make-tube-anchors-for-topology @current-topology 4))))

    ;(draw-tubes 4)
    ;(draw-tube-anchors 0 6)

    ;(draw-billboard [10 0 0] @particle-texture)
    (no-lights)
    (when @draw-empty?
      (draw-empty @tiler-state))

    ;(lights)
    (when @draw-gliders?
      (let [selected-tile ((get-glider 1) :current-tile)]
        ;(draw-neighbours selected-tile)
        ;(draw-curve-boundary-points selected-tile)
        ;(draw-selected-tile selected-tile)
        ))

    ;(fill 255 255 255 255)
    ;(no-stroke)
    ;(draw-obj-textured sky-model @sky-tex)

    (let [pos (vec3-add (vec3-scale (get-glider-pos 1) 1.0) [0 0 1])]
      (no-lights)
      (no-stroke)
      (fill 128 255 192 255)
      (with-translation pos
        (sphere 0.1)))

    (pop-matrix)
    
  )

;  (fill 255)
;  (stroke 0 0 0)
;  (stroke-weight 1)
;  (with-translation [0 0]
;    (scale @model-scale)
;    (draw-verts to-verts))
;
;  (fill 0 255 0)
;  (stroke 0 0 0)
;  (stroke-weight 1)
;  (with-translation [0 0]
;    (scale @model-scale)
;    (draw-verts to-face-centers))

  (pop-matrix)

  ; 2d hud stuff
  (hint :disable-depth-test)
  (no-lights)
  ;(camera)
  ;(ortho)
  ;(hint :disable-depth-test)
  ;(when @draw-info?
  ;  (draw-info 10 (- (height) 370)))

  (camera)

  (when @game-mode?
    (game/render-2d @tiler-state))

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
  ;(fill 255 255 255)
;  (fill 255 255 255 32)
  ;(fill 0 0 0 64)
 ; (rect 0 0, (width) (height))

  ;(display-filter :erode)
  ;(display-filter :posterize 4)
  (no-lights)
  (let [zoom  1.0 
        mx (/ (float (mouse-x)) (width))
        my (/ (float (mouse-y)) (height))
        ar (/ (float (width)) (height))
        px (* (- mx 0.5) ar zoom)
        py (* (- my 0.5) zoom)
        ]
  (.set @edge-shader "framecount" (frame-count))
  (.set @edge-shader "aspect_ratio" (float ar))
  (.set @edge-shader "mousex" (float px))
  (.set @edge-shader "mousey" (float py))
  (.set @edge-shader "zoom" (float zoom))
  (.set @edge-shader "width" (float (width)))
  (.set @edge-shader "height" (float (height)))

  (texture-wrap :repeat)
  (filter-shader @edge-shader)
  )
  ;(display-filter :dilate)
  ;(display-filter :erode)
  ;(display-filter :invert)
  ;(display-filter :blur 1)
  
  ;(fill 0 0 0 64)
  ;(rect 0 0, (width) (height))
 
  ;(draw-vert-numbers @to-verts-screen)
  ;(draw-anchor-numbers @anchor-verts-screen)
    ;(draw-vert-numbers (@current-topology :verts))
  ;(when @game-mode?
  ;  (game/render-2d))

    ; bottom of screen
    ;(draw-tileset-editor [20 (- (height) 180)] @current-tileset 140))
    ;(draw-tileset-editor [1285 20] @current-tileset 140))

  ;(when @draw-graphs?
  ;  (draw-graphs [20 100]))

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
    ;:size [1900 1100]
    ;:size [1440 800]
    :size :fullscreen
    :features [:present :resizable]
    ;:features [:resizable]
    :renderer :opengl
    ;:renderer :p3d
    :key-typed key-typed
    :key-pressed key-pressed
    :key-released key-released
    :mouse-moved mouse-moved
    :mouse-pressed mouse-pressed
    :mouse-released mouse-released))

;(reset! frame ((current-applet) meta :target-obj deref))

;(sketch-start rhombrick)
;(sketch-stop rhombrick)

;(reset! my-applet rhombrick)


;(-main)


;(get-tileset)

;(set-tileset ["2-3-4-"])

;(set-tileset ["--44-4"])
;(set-tileset ["-4-Dd4"])
;(set-tileset ["---b13B-----"])

; self-compatible tiles
;(set-tileset ["-4-Dd4"
;              "--43-4"
;              "-4-D-d"
;              "-3-cC3"
;              "---344"
;              "--33-4"
;              "-3C3-c"
;              "--3-Dd"
;              "-3-3-4"
;              "--3-43"
;              "-3C43c"
;              "--3-33"
;              "---cC3"
;              ])

(comment



(set-tileset ["dD-4D-" "d44---"])

(set-tileset ["d-4-D-" "d--D--"])

;(set-tileset ["--BB-B" "3-BB-B" "-3BB-B" "b-b-b-" "3-3---" "33----" "3-3-3-"])

;(set-tileset ["--CC-C" "3-CC-C" "-3CC-C" "c-c-c-" "3-3---" "33----" "3-3-3-"])
;(set-tileset ["--DD-D" "3-DD-D" "-3DD-D" "d-d-d-" "3-3---" "33----" "3-3-3-"])
(set-tileset ["-33B3D" "-dbB3b" "-33D-d" "----33"])
;(set-tileset ["----44"]);
(set-tileset ["----12" "----23" "----31"])

(set-tileset ["-c--12" "-C--23" "-2--31"])

(set-tileset ["4b3c2d" "4B3C2D" "4-b---" "3-c---" "2-d---"  "B--4--" "C--4--" "D--4--" "4-c-d-"])

(set-tileset ["1a--A1"])


(set-tileset [
              "d-d-d-"
              ;"d1d1d1"

              "D1-C-1"
              ;"c--B--"
              ;"b--2--"
              "c--B--"
              "b--2--"


              ;"411d11"

              "111111"
              ;"11111-"

              "1aaa1-"
              ;"AAaa--"
              ;"aaAA--"
              ;"aA----"
              ;"aa----"
              "A--1--"
              ;"a--1--"

              ;"a-A---"
              ;"Aa----"
              ;"aA----"

              ;"A--A--"
              ;"a--a--"
              ;"a-A-a-"
              ;"A-a-a-"

              ;"A-a-1-"
              ;"A-1-1-"
              ;"A-----"
              ;"111-1-"
              ;"11-11-"
              "1111--"
              ;"1-1-1-"
              ;"1--1--"
              ;"111---"
              ;"1-1---"
              ;"11----"
              ;"1-----"
              ])


(set-tileset [;"--d4-4"
              "11d4-4"

              ;"-433-D"
              "1433-D"
              "3--3--"

              "111111"
              "11111-"
              "1111--"
              "111---"
              "11----"
              ;"1-----"
              ])

; hex stars
(set-tileset [
              "aaaaaa"
              "1A1-b-"
              ;"1A1-B-"
              "2B2-c-"
              ;"2B2-C-"
              "3C3-d-"
              ;"3C3-D-"


              ;"1--1--"
              "2--2--"
              "3--3--"
              ;"4114--"
              ;"3113--"

              ;"111111"
              ;"11111-"
              ;"1111--"
              ;"111---"
              ;"11----"
              ;"1-----"

              ;"a-----"
              "b-----"
              ;"A--a--"
              ;"B--b--"
              ;"b-----"
              ;"33----"
              ;"3-----"
              ;"4--4--"
              ;"4-4---"
              ;"44----"
              ;"4-----"
              ;"4D4---"
              ;"4d4---"

              ;"3c3-3-"
              ;"A--1--"
              ;"B--2--"
              ;"C--3--"
              "D--4--"
              ;"d--4--"
              ;"dDddDd"
              ;"dDd-D-"
              ;"dDd---"
              ;"d-D-d-"
              ;"D-d-D-"


])


(set-tileset [
              "d-d-D-"
              "D-D-d-"
              ;"D--4--"
              "d-D---"
              "DDD---"
              "ddDD--"
              "dDdDd-"
              "dddddd"
              "DDDDDD"
              ])

; inside to outside pattern
(set-tileset ["aaaaaa"
              "b--A--"
              "c--B--"

              "1A1-b-"
              "2B2-c-"
              "3C3-d-"
              ;"4D4-4-"
              "2--2--"
              "2112--"
              ;"C-----"
              "cCc---"
              ;"3C3---"
              ;"dDddDd"
              "d-d-D-"
              ;"D-D-d-"
              "D--4--"
              ;"4--4--"
              ;"3113--"
              ;"4114--"
              ;"3--3--"
              ;"b-1-1-"
              ;"111111"
              ;"11111-"
              ;"1111--"
              ;"111---"
              ;"11----"
              ;"1-----"

              ])


; hex 4321 pattern
(set-tileset ["--cC-D"
              "d-b-b1"

              ;"D--4--"
              ;"d11411"
              ;"d13413"
              ;"d33433"
              ;"d-----"


              "BB3223"
              ;"BB3--3"

              "3-3---"
              "3-13--"
              ;"2--2--"
              "2222--"
              "22222-"
              "222222"
              "222-2-"
              ;"22----"
              ;"2-2---"
              "2-2-11"
              "2-2-1-"
              "2-2--1"

              ;"2-2--1"
              ;"212---"
              ;"2-2---"

;              "111111"
;              "11111-"
;              "1111--"
;              "111---"
;              "11----"
              ;"1-----"
;              "1-1---"
;              "1-11-1"

;              "1-1-1-"
              ])



(set-tileset ["--cC-D"
              ;"D-d-3-"
              ;"d-d-d-"
              ;"D-----"
              "d-b-b-"
              "BB3--3"
              ;"BB3--3"
              ;"3-3---"
              ;"3--3--"
              "3-----"])

; worms in hexagons
(set-tileset [;"--aA-d"
              "--cC-D"
              ;"--cC1D"

              ;"---D-C"
              ;"--c--D"
              ;"-----D"

              ;"d11411"
              ;"d13413"
              ;"d33433"

              ;"d--4--" ; extender
              ;"4--4--" ; extender



              ;"1-----"
              ;"3-----"
              ;"4-----"

              "d-b-b1"
              ;"d--4-1"

              "BB----"
              ;"BB3--3"
              "BB3113"
              ;"BB-22-"

              "111111"
              "1--1--"
              ;"1-----"
              "11----"
              ;"1-1---"
              "1-1-1-"
              "1111--"
              ;"222---"
              "2--2--"
              "2-2---"
              ;"2-1-1-"
              ;"2-2-2-"
              ;"222-2-"
              ;"222222"

              ;"BB3333"
              ;"3333--"
              ;"333---"
              ;"3-3-3-"
              ;"3-----"
              "3-3---"
              "3-13--"
              ;"33-3--"
              ])


(set-tileset ["--cC-D"
              "d-b-b1"
              "BB----"

              "1-----"])



;@shape-2d-cache


(set-tileset ["4c4c4c" "c-C-C-" "4--4--" "C-----" "C--5--" "555555" ]  )

(set-tileset ["4c4c4c"  "d-D-C-" "4--4--"
              "bbbbbb" "B--C--"
              "C-2-2-" "c-2-2-"
              "4c4---" "c4c---"
              "cc-cc-"
              "c-----"
              "C-----"

              ]

             )

(set-tileset ["4c4c4c" "c-c-c-" "c-C-C-" "4--4--"  "CC-CC-"])


(set-tileset ["--d4-4"
              "-433-D"
              ;"-43c--"
              ;"--C3-D"
              ])
;(get-facecode-shape-2d "3-3-3-" @current-topology 4)


(set-tileset ["3-33--" "33----"])

(set-tileset ["ccCC--" "cC-c-C"
              "ccCCaa" "cCacaC"
              ;"A-A-A-"
              ;"AA----"
              "A--1--"
              "a--1--"
              "1-1-1-"
              "1-----"

              ])


(set-tileset ["4-4-4-" "-44d-D"])

(set-tileset ca-rule-110)

(init-tileset-colors-ca (get-tileset))



)







