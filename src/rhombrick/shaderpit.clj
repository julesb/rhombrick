(ns rhombrick.shaderpit
  (:require [quil.core :as q]
            [quil.middleware :as m])
  (:use [rhombrick.vector])
  (:import java.awt.event.KeyEvent)
  )

(def ^:dynamic console-font)
;(def render-paused? (atom false))
(def edge-shader (atom nil))
(def test-shader (atom nil))
(def color-shader (atom nil))
(def texture-shader (atom nil))
(def feedback-shader (atom nil))
(def ray-shader (atom nil))
(def tex1 (atom nil))
(def PI Math/PI)

; =========================================================================

(def initial-camera {
  :pos [128.0 0.0 256.0]
  :lookat [0.0 0.0 0.0]
  :vpn [0.0 0.0 -1.0]
  :speed 1.5
})

(def initial-params {
  :blend_coef 0.5
})

(def initial-state {
  :keys-down #{}
  :mouse-position [0 0]
  :aspect-ratio 1.0
  :render-paused? false
  :camera initial-camera
  :params initial-params
})




(defn setup []
    (q/smooth)
    (q/texture-mode :normal)
    (q/texture-wrap :repeat)
    (q/noise-detail 2)
    (q/hint :disable-depth-test)
    ;(frame-rate 120)
    (def console-font (q/load-font "data/FreeMono-16.vlw"))
;    (def console-font (q/load-font "ScalaSans-Caps-32.vlw"))
    (reset! edge-shader (q/load-shader "data/edges.glsl"))
    (reset! tex1 (q/load-image "testpattern4po6.png"))
    (reset! test-shader (q/load-shader "data/test.frag"))
    (reset! color-shader (q/load-shader "data/colorfrag.glsl" "data/colorvert.glsl"))
    (reset! ray-shader (q/load-shader "data/raymarch.glsl" ))
    ;(reset! texture-shader (load-shader "data/texfrag.glsl" "data/texvert.glsl"))
    (reset! texture-shader (q/load-shader "data/texfrag.glsl"))
    (reset! feedback-shader (q/load-shader "data/feedbackfrag.glsl"))
    ;(reset! aspect-ratio  (/ (float (q/width)) (q/height)))

    (-> initial-state
         (assoc :aspect-ratio (/ (float (q/width)) (q/height))))
  )


(defn update-uniforms! [state shader] 
  (when state
    (let [mp (get state :mouse-position [0 0])
          ar (state :aspect-ratio)
          cam-pos (get-in state [:camera :pos])
          cam-lookat (get-in state [:camera :lookat])
          ]
      (.set shader "framecount" (float (q/frame-count)))
      (.set shader "aspect_ratio" (float ar))
      ;(when (mouse-pressed?)
        (.set shader "mousex" (float (mp 0)))
        (.set shader "mousey" (float (mp 1)))
      ;  )
      (.set shader "swidth" (float (q/width)))
      (.set shader "sheight" (float (q/height)))
      (.set shader "cam_pos" (float (cam-pos 0)) 
                             (float (cam-pos 1))
                             (float (cam-pos 2)))
      (.set shader "cam_lookat" (float (cam-lookat 0)) 
                                (float (cam-lookat 1))
                                (float (cam-lookat 2)))
      (.set shader "blend_coef" (float (get-in state [:params :blend_coef])))
      )
    state))


;(def speed 0.025)
;(def turbospeed 1.5)

(defn camera-mouse-update [state]
  (let [[mx my] (vec2-mul (vec2-sub (state :mouse-position) [0.5 0.5])
                          [(* PI 2.0) (* PI 0.99)])
        pos (get-in state [:camera :pos] [0.0 0.0 0.0])
        vpn (vec3-normalize [(* (Math/cos my) (Math/cos mx))
                             (Math/sin my)
                             (* (Math/cos my) (Math/sin mx))])
        lookat (vec3-add pos (vec3-scale vpn 6.0))
        new-cam (-> (state :camera)
                    (assoc :vpn vpn)
                    (assoc :lookat lookat)) ]
  (-> state
      (assoc :camera new-cam))))


(defn do-key-movement [state keychar]
  (let [pos-old  (get-in state [:camera :pos] [0.0 0.0 0.0])
        wup [0.0 -1.0 0.0] ; world up
        vpn (get-in state [:camera :vpn])
        vpv (vec3-normalize (vec3-cross (get-in state [:camera :vpn]) [0.0 -1.0 0.0]))
        speed (get-in state [:camera :speed])
        ;vpu (vec3-normalize (vec3-cross vpn vpv)) ;viewplane up
        key-movement-map {
          ;\W (fn [s] (assoc-in s [:camera :pos] (vec3-add pos-old (vec3-scale vpn speed))))
          ;\S (fn [s] (assoc-in s [:camera :pos] (vec3-sub pos-old (vec3-scale vpn speed))))
          ;\A (fn [s] (assoc-in s [:camera :pos] (vec3-add pos-old (vec3-scale vpv speed))))
          ;\D (fn [s] (assoc-in s [:camera :pos] (vec3-sub pos-old (vec3-scale vpv speed))))
          \w (fn [s] (assoc-in s [:camera :pos] (vec3-add pos-old (vec3-scale vpn speed))))
          \s (fn [s] (assoc-in s [:camera :pos] (vec3-sub pos-old (vec3-scale vpn speed))))
          \a (fn [s] (assoc-in s [:camera :pos] (vec3-add pos-old (vec3-scale vpv speed))))
          \d (fn [s] (assoc-in s [:camera :pos] (vec3-sub pos-old (vec3-scale vpv speed))))
          \e (fn [s] (assoc-in s [:camera :pos] (vec3-sub pos-old (vec3-scale wup speed))))
          \c (fn [s] (assoc-in s [:camera :pos] (vec3-add pos-old (vec3-scale wup speed))))
          ;\E (fn [s] (assoc-in s [:camera :pos] (vec3-sub pos-old (vec3-scale wup speed))))
          ;\C (fn [s] (assoc-in s [:camera :pos] (vec3-add pos-old (vec3-scale wup speed))))
          \b (fn [s] (update-in s [:params :blend_coef] #(- % 0.01)))
          \n (fn [s] (update-in s [:params :blend_coef] #(+ % 0.01)))
          \1 (fn [s] (update-in s [:camera :speed] #(* % 0.9)))
          \2 (fn [s] (update-in s [:camera :speed] #(/ % 0.9)))
         }]
  (if (contains? key-movement-map keychar)
    (-> ((key-movement-map keychar) state)
        (camera-mouse-update))
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


(defn key-typed [state event]
  (case (event :raw-key)
    \p (do
         (if (state :render-paused?)
           (q/start-loop)
           (q/no-loop))
        (update-in state [:render-paused?] not))
    \# (do
         (q/save-frame)
         state)
    \r (do (-> initial-state
               (assoc :aspect-ratio (/ (float (q/width)) (q/height)))))
    \0 (do (-> initial-state
               (assoc :aspect-ratio (/ (float (q/width)) (q/height)))
               (assoc-in [:camera :pos] [0.0 0.0 0.0])))
    \` (do
         (reset! edge-shader (q/load-shader "data/edges.glsl"))
         (reset! texture-shader (q/load-shader "data/texfrag.glsl"))
         (reset! feedback-shader (q/load-shader "data/feedbackfrag.glsl"))
         (reset! ray-shader (q/load-shader "data/raymarch.glsl" ))
         state)
    state))


(defn mouse-moved [state event]
    (-> state
        (assoc :mouse-position [(/ (event :x) (q/width))
                                (/ (event :y) (q/height))])
        (camera-mouse-update)
        ))


(defn mouse-dragged [state event]
    (-> state
        (assoc :mouse-position [(event :x) (event :y)])))




(defn update [state]
  (-> state
      (do-movement-keys)
      ;(update-uniforms! @texture-shader)
      ;(update-uniforms! @feedback-shader)
      (update-uniforms! @ray-shader)
  ))


(defn draw-info [state x y]
  (q/text-font console-font)
  (let [line-space 24
        ar (get state :aspect-ratio 1.0)
        [mx my] (state :mouse-position)
        zoom (get state :zoom 1.0)
        pos (get-in state [:camera :pos] [0.0 0.0 0.0])
        speed (get-in state [:camera :speed] 0.0)
        lines [
               ;(str "state: " state)
               (str "pos: " (vec3-format pos))
               (str (format "mouse: [%.2f %.2f]" (float mx) (float my)))
               (str (format "speed: %.6f" speed))
               (str (format "ar: %.2f" ar))
               ;(str "camera: " (state :camera))
               (str (format "fps: %.2f" (float (q/current-frame-rate))))
               ]]
    (q/fill 255 255 255 255)
    (doseq [i (range (count lines))]
      (q/text (lines i) x (+ y (* i line-space))))))


(defn draw-quad [ar]
  (q/begin-shape :quads)
    ;(q/texture @tex1)
    (q/shader @ray-shader)
    ;(q/shader @texture-shader)
    ;(q/shader @color-shader)
    (q/vertex -1 -1 0 0)
    (q/vertex  1 -1 ar 0)
    (q/vertex  1  1 ar 1)
    (q/vertex -1  1 0 1)
  (q/end-shape))



(defn draw [state]
  ;(q/background 0 0 0)
  (q/fill 0 0 0 196)
  (q/ortho)
  (q/no-lights)
  (let [c [(* (q/width) 0.5)
           (* (q/height) 0.5)]]
    (q/with-translation c
      (q/scale (c 0) (c 1))
      (q/fill 0 0 0)
      (q/stroke 0 0 0 255)
      (q/stroke-weight 0.125)
      (q/no-stroke)
      (draw-quad (get state :aspect-ratio 1.0))

      )
    )
  (q/reset-shader) 
  (draw-info state 32 (- (q/height) 150))
  ;(q/filter-shader @edge-shader)
  ;(q/filter-shader @feedback-shader)
  )


(defn -main [& args]
  (q/defsketch shaderpit
    :title "shader sandpit"
    :setup setup
    :draw draw
    ;:size [1900 1100]
    ;:size [1440 800]
    :size :fullscreen
    :features [:present
               :resizable]
 
    :renderer :p3d
    ;:renderer :opengl
    :key-typed key-typed
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
