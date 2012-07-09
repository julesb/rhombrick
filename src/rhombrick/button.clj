(ns rhombrick.button
  (:use [quil.core]))

(def current-tileset (atom #{}))

(def editor-visible? (atom false))
(def button-width 64)
(def button-height 64)
(def button-space 8)

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


(defn add-to-current-tileset [code]
  (swap! current-tileset conj code))

(defn remove-from-current-tileset [code]
  (swap! current-tileset disj code))

(defn init-editor []
  (reset! current-tileset #{}))

(defn set-current-tileset [tileset]
  (reset! current-tileset tileset))

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
          (stroke 255 255 255 192)
          (fill 255 128 128 255))
        (do
          ; just hot
          (stroke 255 255 255 192)
          (fill 128 128 255 192)))
      (do
        ; not hot, may be active
        (stroke 255 255 255 192)
        (fill 128 128 128 128)))

    (rect x y button-width button-height)

    (if (and
          (not (@ui-state :mouse-down))
          (= (@ui-state :hot-item) [x y])
          (= (@ui-state :active-item) [x y]))
      true
      false))


(defn draw-buttons [pos]
  (stroke-weight 1)
  (ui-prepare)
  (doseq [xi (range 10)
          yi (range 10)]
    (let [x (+ (pos 0)
               (* xi button-width)
               (* xi button-space))
          y (+ (pos 1)
               (* yi button-height)
               (* yi button-space))]
    (if (button x y "111111111111")
      (println @ui-state))))
  (ui-finish))


(defn draw-tile-groups []
  )
