(ns rhombrick.button
  (:use [quil.core]
        [rhombrick.tiling-render]))

; simple immediate mode ui button, inspiration from http://iki.fi/sol/imgui/ 

(def button-width 32)
(def button-height 32)
(def button-space 8)

(def button-fill [0 0 0 152])
(def button-stroke [64 64 64 255])

(def button-fill-hot [128 128 255 192])
(def button-stroke-hot [0 0 192 192])

(def button-fill-active [255 255 0 192])
(def button-stroke-active [255 255 255 192])

(def default-button-color {:fill [0 0 0 152]
                           :stroke [64 64 64 255]
                           :fill-hot [128 128 255 192]
                           :stroke-hot [0 0 192 192]
                           :fill-active [255 255 0 192]
                           :stroke-active [255 255 255 192]
                           })

(def ui-state (atom {:mouse-x 0
                     :mouse-y 0
                     :mouse-down false
                     :hot-item 0
                     :active-item 0}))


(defn init-ui-state []
  (reset! ui-state {:mouse-x 0
                    :mouse-y 0
                    :mouse-down false
                    :hot-item 0
                    :active-item 0}))


(defn update-ui-state [k v]
  (swap! ui-state assoc k v))


(defn ui-prepare []
  (update-ui-state :hot-item 0))


(defn ui-finish []
  (if (not (@ui-state :mouse-down))
    (update-ui-state :active-item 0)
    (if (= (@ui-state :active-item) 0)
      (update-ui-state :active-item -1))))


(defn region-hit [x y w h]
  (not (or
    (< (@ui-state :mouse-x) x)
    (< (@ui-state :mouse-y) y)
    (> (@ui-state :mouse-x) (+ w x))
    (> (@ui-state :mouse-y) (+ h y)))))


(defn button [x y w h colors code]
  (if (region-hit x y w h)
    (do
      (update-ui-state :hot-item [x y])
      (if (and (= (@ui-state :active-item) 0)
               (@ui-state :mouse-down))
        (update-ui-state :active-item [x y]))))
  (if (= (@ui-state :hot-item) [x y])
    (if (= (@ui-state :active-item) [x y])
      (do
        ; hot and active
        (apply stroke (colors :stroke-active))
        (apply fill (colors :fill-active)))
      (do
        ; just hot
        (apply stroke (colors :stroke-hot))
        (apply fill (colors :fill-hot))
        ))
    (do
      ; not hot, may be active
      (apply stroke (colors :stroke))
      (apply fill (colors :fill))))
  (rect x y w h)
  (and (not (@ui-state :mouse-down))
       (= (@ui-state :hot-item) [x y])
       (= (@ui-state :active-item) [x y])))


