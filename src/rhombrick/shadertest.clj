(ns rhombrick.shadertest 
  (:use [quil.core :exclude [target-frame-rate]]
        [quil.applet]
        [rhombrick.vector]
        )
  (:import java.awt.event.KeyEvent)
  )
(def ^:dynamic console-font)
(def ^:dynamic edge-shader)
(def keys-down (atom #{}))
(def mouse-position (atom [0 0]))
(def aspect-ratio (atom [0 0]))
(def view-scale (atom 1.0))
(def view-offset (atom [0 0]))

(def render-paused? (atom false))
(def test-shader (atom nil))
(def color-shader (atom nil))
(def texture-shader (atom nil))
(def tex1 (atom nil))
; =========================================================================


(defn setup []
    (smooth)
    ;(stroke-cap :round)
    ;(no-stroke)
    (fill 226)
    (texture-mode :normal)
    (texture-wrap :repeat)
;    (def console-font (load-font "data/FreeMono-16.vlw"))
    (def console-font (load-font "ScalaSans-Caps-32.vlw"))
    (def edge-shader (load-shader "data/edges.glsl"))
    (reset! tex1 (load-image "testpattern4po6.png"))
    (reset! test-shader (load-shader "data/test.frag"))
    (reset! color-shader (load-shader "data/colorfrag.glsl" "data/colorvert.glsl"))
    ;(reset! texture-shader (load-shader "data/texfrag.glsl" "data/texvert.glsl"))
    (reset! texture-shader (load-shader "data/texfrag.glsl"))
    (reset! aspect-ratio  (/ (float (width)) (height)))
  )


(defn update-uniforms [shad] 
  (let [zoom  @view-scale
        mx (/ (float (mouse-x)) (width))
        my (/ (float (mouse-y)) (height))
        ar (/ (float (width)) (height))
        px (* (- mx 0.5) ar zoom)
        py (* (- my 0.5) zoom)
        vx (/ (float (@view-offset 0)) (width))
        vy (/ (float (@view-offset 1)) (height))
        ]
    (.set shad "framecount" (frame-count))
    (.set shad "viewx" (float vx))
    (.set shad "viewy" (float vy))
    (.set shad "aspect_ratio" (float ar))
    (.set shad "mousex" (float px))
    (.set shad "mousey" (float py))
    (.set shad "zoom" (float zoom))
    ;(.set shad "width" (float (width)))
    ;(.set shad "height" (float (height)))
  ))


(def speed 10.0)

(def key-command-map
  {
   \e #(do
         (def edge-shader (load-shader "data/edges.glsl"))
         )


   \p #(do
          (if @render-paused?
            (start-loop)
            (no-loop)
            )
          (swap! render-paused? not)
          )
   \` #(do
          (println "loading shader")
          (reset-shader)
          ;(reset! color-shader (load-shader "data/colorfrag.glsl" "data/colorvert.glsl"))
          ;(reset! texture-shader (load-shader "data/texfrag.glsl" "data/texvert.glsl"))
          (reset! texture-shader (load-shader "data/texfrag.glsl"))
          )
   })

(def key-movement-map
  {
   \w #(do
         (reset! view-offset (vec2-add @view-offset [0.0 (* speed @view-scale)]))
         )
   \s #(do
         (reset! view-offset (vec2-sub @view-offset [0.0 (* speed @view-scale)]))
         )
   \a #(do
         (reset! view-offset (vec2-add @view-offset [(* speed @view-scale @aspect-ratio) 0.0]))
         )
   \d #(do
         (reset! view-offset (vec2-sub @view-offset [(* speed @view-scale @aspect-ratio) 0.0]))
         )
   \, #(reset! view-scale (* 0.99 @view-scale))
   \. #(reset! view-scale (* 1.01 @view-scale))
   
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
    (if (and coded? (contains? key-movement-map the-key-pressed))
      ((key-movement-map the-key-pressed))
      (swap! keys-down conj the-raw-key)))
  (println @keys-down)
  
  )


(defn key-released []
  (swap! keys-down disj (raw-key))
  (println @keys-down))


(defn mouse-moved []
  (let [x (mouse-x) y (mouse-y)]
    (reset! mouse-position [x y])
    ;(reset! view-offset @mouse-position)
    ))

(defn draw-info [x y]
  (text-font console-font)
  (let [line-space 30 
        lines [
               (str (format "view: [%.2f %.2f]" (@view-offset 0) (@view-offset 1)))
               (str "mouse: " @mouse-position)
               (str (format "zoom: %.3f" @view-scale))
               (str "fps: " (current-frame-rate))
               (str "ar: " @aspect-ratio)
               ]]
    (fill 255 255 255 255)
    (doseq [i (range (count lines))]
      (text (lines i) x (+ y (* i line-space))))))


(defn draw-quad []
  (let [ar @aspect-ratio]
    (begin-shape :quads)
      (texture @tex1)
      (shader @texture-shader)
      ;(shader @color-shader)
      (vertex -1 -1 0 0)
      (vertex  1 -1 ar 0)
      (vertex  1  1 ar 1)
      (vertex -1  1 0 1)
    (end-shape)))


(defn draw []
  (do-movement-keys)
  (update-uniforms @texture-shader)
  (background 0 0 0)
  (fill 0 0 0 196)
  (ortho)
  (hint :disable-depth-test)
  (texture-wrap :clamp)
  (no-lights)
  (let [c [(* (width) 0.5)
           (* (height) 0.5)]]
    (with-translation c
      (scale (c 0) (c 1))
      (fill 0 0 0)
      (stroke 0 255 0)
      (stroke-weight 0.05)
      (draw-quad)

      )
    )
  (reset-shader) 
  (draw-info 32 (- (height) 150))

  )


(defn -main [& args]
  (defsketch shadertest
    :title "shader test"
    :setup setup
    :draw draw
    ;:size [1900 1100]
    :size [1440 800]
    :renderer :p3d
;    :renderer :opengl
    :key-typed key-typed
    :key-pressed key-pressed
    :key-released key-released
    :mouse-moved mouse-moved
;    :mouse-pressed mouse-pressed
;    :mouse-released mouse-released
    ))


;(-main)
