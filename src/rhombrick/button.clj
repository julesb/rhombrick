(ns rhombrick.button
  (:use [quil.core]
        [rhombrick.tiling-render]))


(def button-width 32)
(def button-height 32)
(def button-space 8)

(def button-fill [0 0 0 128])
(def button-stroke [0 0 0 255])

(def button-fill-hot [128 128 255 192])
(def button-stroke-hot [0 0 192 192])

(def button-fill-active [255 255 0 192])
(def button-stroke-active [255 255 255 192])



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


; _______________________________________________________________________


(defn button [x y code]
  (if (region-hit x y button-width button-height)
    (do
      (update-ui-state :hot-item [x y])
      (if (and 
            (= (@ui-state :active-item) 0)
            (@ui-state :mouse-down))
        (update-ui-state :active-item [x y]))
      ))

    (if (= (@ui-state :hot-item) [x y])
      (if (= (@ui-state :active-item) [x y])
        (do
          ; hot and active
          (apply stroke button-stroke-active)
          (apply fill button-fill-active))
          ;(stroke 255 255 255 192)
          ;(fill 255 128 128 255))
        (do
          ; just hot
          (apply stroke button-stroke-hot)
          (apply fill button-fill-hot)))
          ;(stroke 255 255 255 192)
          ;(fill 128 128 255 192)))
      (do
        ; not hot, may be active
        (apply stroke button-stroke)
        (apply fill button-fill)))
        ;(stroke 255 255 255 192)
        ;(fill 0 0 0 192)))

    (rect x y button-width button-height)

    (if
      (and
        (not (@ui-state :mouse-down))
        (= (@ui-state :hot-item) [x y])
        (= (@ui-state :active-item) [x y]))
      true
      false))



