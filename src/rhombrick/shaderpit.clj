(ns rhombrick.shaderpit
  (:require [quil.core :as q]
            [quil.middleware :as m])
  (:use [rhombrick.vector])
  (:import java.awt.event.KeyEvent)
  )

(def ^:dynamic console-font)
(def ^:dynamic edge-shader)
;(def render-paused? (atom false))
(def test-shader (atom nil))
(def color-shader (atom nil))
(def texture-shader (atom nil))
(def ray-shader (atom nil))
(def tex1 (atom nil))
; =========================================================================

(def initial-state {
  :keys-down #{}
  :mouse-position [0 0]
  :aspect-ratio 1.0
  :view-scale 1.0
  :view-offset [0 0]
  :render-paused? false
})




(defn setup []
    (q/smooth)
    (q/texture-mode :normal)
    (q/texture-wrap :repeat)
    ;(frame-rate 120)
    (def console-font (q/load-font "data/FreeMono-16.vlw"))
;    (def console-font (q/load-font "ScalaSans-Caps-32.vlw"))
    (def edge-shader (q/load-shader "data/edges.glsl"))
    (reset! tex1 (q/load-image "testpattern4po6.png"))
    (reset! test-shader (q/load-shader "data/test.frag"))
    (reset! color-shader (q/load-shader "data/colorfrag.glsl" "data/colorvert.glsl"))
    (reset! ray-shader (q/load-shader "data/raymarch.glsl" ))
    ;(reset! texture-shader (load-shader "data/texfrag.glsl" "data/texvert.glsl"))
    (reset! texture-shader (q/load-shader "data/texfrag.glsl"))
    ;(reset! aspect-ratio  (/ (float (q/width)) (q/height)))

    (-> initial-state
         (assoc :aspect-ratio (/ (float (q/width)) (q/height))))
  )





(defn update-uniforms! [state shader] 
    (let [zoom  (get state :view-scale 1.0) ;@view-scale
          mp (get state :mouse-position [0 0])
          mx (/ (float (mp 0)) (q/width))
          my (/ (float (mp 1)) (q/height))
          ar (/ (float (q/width)) (q/height))
          px (* (- mx 0.5) ar zoom)
          py (* (- my 0.5) zoom)
          [vx vy] (vec2-scale (get state :view-offset [0 0])
                              (get state :aspect-ratio 1.0))
          ]
      (.set shader "framecount" (q/frame-count))
      (.set shader "viewx" (float vx))
      (.set shader "viewy" (float vy))
      (.set shader "aspect_ratio" (float ar))
      (.set shader "zoom" (float zoom))
      ;(when (mouse-pressed?)
        (.set shader "mousex" (float px))
        (.set shader "mousey" (float py))
      ;  )
      ;(.set shad "width" (float (width)))
      ;(.set shad "height" (float (height)))
    ;)
    ))


(def speed 0.005)


(def key-command-map
  {
   \e #(do
         (def edge-shader (q/load-shader "data/edges.glsl"))
         )
   ;\p #(do
   ;       (if @render-paused?
   ;         (q/start-loop)
   ;         (q/no-loop))
   ;       (swap! render-paused? not)
   ;       )
   \` #(do
          (println "loading shader")
          (q/reset-shader)
          ;(reset! color-shader (load-shader "data/colorfrag.glsl" "data/colorvert.glsl"))
          ;(reset! texture-shader (q/load-shader "data/texfrag.glsl" "data/texvert.glsl"))
          (reset! texture-shader (q/load-shader "data/texfrag.glsl"))
          ;(reset! ray-shader (q/load-shader "data/raymarch.glsl"))
          )
   ;\0 #(do
   ;      (reset! view-offset [0 0])
   ;     )
   })


(defn do-key-movement [state keychar]
  (let [vs (get state :view-scale 1.0)
        vo (get state :view-offset [0 0])
        ar (get state :aspect-ratio 1.0)
        key-movement-map {
          \w (fn [s] (assoc s :view-offset (vec2-add vo [0.0 (* speed vs)])))
          \s (fn [s] (assoc s :view-offset (vec2-sub vo [0.0 (* speed vs)])))
          \a (fn [s] (assoc s :view-offset (vec2-add vo [(* speed vs ) 0.0])))
          \d (fn [s] (assoc s :view-offset (vec2-sub vo [(* speed vs ) 0.0])))
          \, (fn [s] (assoc s :view-scale (* 0.99 vs)))
          \. (fn [s] (assoc s :view-scale (* 1.01 vs)))
          \p (fn [s] (assoc s :render-paused? (not (s :render-paused?))))
          \r (fn [s] initial-state)
         }]
  (if (contains? key-movement-map keychar)
    ((key-movement-map keychar) state)
    state)))


(defn do-movement-keys [state & keys-down]
  (if (nil? keys-down)
    (recur state (state :keys-down))
    (if (<= (count keys-down) 0)
      state
      (recur (do-key-movement state (first keys-down))
             (rest keys-down)))))


(defn key-pressed [state event]
  (let [the-raw-key (event :raw-key)
        the-key-code (event :key-code)
        coded? (= processing.core.PConstants/CODED (int the-raw-key))
        the-key-pressed (if coded? the-key-code the-raw-key) ]
    (if coded?
      state
      (-> state
          (assoc :keys-down (conj (state :keys-down) the-raw-key))))))
      

(defn key-released [state]
  (-> state
      (assoc :keys-down (disj (state :keys-down) (q/raw-key)))))


(defn mouse-moved [state event]
    (-> state
        (assoc :mouse-position [(event :x) (event :y)])))


(defn mouse-dragged [state event]
    (-> state
        (assoc :mouse-position [(event :x) (event :y)])))


(defn update [state]
  (do-movement-keys state))


(defn draw-info [state x y]
  (q/text-font console-font)
  (let [line-space 24
        [vx vy] (get state :view-offset [0 0])
        vs (get state :view-scale 1.0)
        [mx my] (get state :mouse-position [0 0])
        zoom (get state :zoom 1.0)
        ar (get state :aspect-ratio 1.0)
        lines [
               (str "state: " state)
               (str (format "view: [%.2f %.2f]" (float vx) (float vy)))
               (str (format "mouse: [%.0f %.0f]" (float mx) (float my)))
               (str (format "zoom: %.3f" vs))
               (str (format "ar: %.2f" ar))
               (str "fps: " (q/current-frame-rate))
               ]]
    (q/fill 255 255 255 255)
    (doseq [i (range (count lines))]
      (q/text (lines i) x (+ y (* i line-space))))))


;(defn auto-move []
;  (let [nr 200.0
;        fr-count (q/frame-count)
;        n1 (* 2.0 (- (q/noise (/ (+ fr-count 8474892) nr)) 0.5))
;        n2 (* 2.0 (- (q/noise (/ (+ fr-count 3988479) nr)) 0.5))
;        n3 (* 2.0 (- (q/noise (/ (+ fr-count 2780374) nr)) 0.5))
;        n4 (* 2.0 (- (q/noise (/ (+ fr-count 5047583) nr)) 0.5))
;        ]
;    (reset! view-offset [(* n1 (q/height) @view-scale 1.5)
;                         (* n2 (q/height) @view-scale 1.5)])
;    ;(reset! mouse-position [(* n3 (height) @view-scale 0.5)
;    ;                        (* n4 (height) @view-scale 0.5)])
;  
;  ))


(defn draw-quad [ar]
  (q/begin-shape :quads)
    ;(q/texture @tex1)
    ;(q/shader @ray-shader)
    (q/shader @texture-shader)
    ;(q/shader @color-shader)
    (q/vertex -1 -1 0 0)
    (q/vertex  1 -1 ar 0)
    (q/vertex  1  1 ar 1)
    (q/vertex -1  1 0 1)
  (q/end-shape))



(defn draw [state]
  (update-uniforms! state @texture-shader)
  ;(update-uniforms @ray-shader)
  (q/background 0 0 0)
  (q/fill 0 0 0 196)
  (q/ortho)
  (q/noise-detail 2)
  (q/hint :disable-depth-test)
  (q/texture-wrap :repeat)
  (q/no-lights)
  (let [c [(* (q/width) 0.5)
           (* (q/height) 0.5)]]
    (q/with-translation c
      (q/scale (c 0) (c 1))
      (q/fill 0 0 0)
      (q/stroke 255 255 255 255)
      (q/stroke-weight 0.5)
      (q/no-stroke)
      (draw-quad (get state :aspect-ratio 1.0))

      )
    )
  (q/reset-shader) 
  (draw-info state 32 (- (q/height) 150))
  ;(filter-shader edge-shader)
  )


(defn -main [& args]
  (q/defsketch shaderpit
    :title "shader sandpit"
    :setup setup
    :draw draw
    ;:size [1900 1100]
    :size [1440 800]
    ;:size :fullscreen
    :features [;:present 
               :resizable]
 
    :renderer :p3d
;    :renderer :opengl
    ;:key-typed key-typed
    :update update
    :key-pressed key-pressed
    :key-released key-released
    :mouse-moved mouse-moved
    :mouse-dragged mouse-dragged
;    :mouse-pressed mouse-pressed
;    :mouse-released mouse-released
    :middleware [m/fun-mode]
    ))


;(-main)
