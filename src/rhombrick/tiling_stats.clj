(ns rhombrick.tiling-stats

  )



(def stats-buffer-length 200)
(def stats-tile-count (atom []))
;(def stats-empty-count (atom ()))
(def stats-dead-count (atom []))
(def stats-backtrack (atom []))
(def stats-iter-time (atom []))
(def stats-efficiency (atom []))

(defn append-stats-buffer! [buf new-val]
  (swap! buf conj new-val)
    (when (> (count @buf) stats-buffer-length)
      (swap! buf #(vec (rest %)))))


(defn adapt-backtrack-params []
  (let [current-tilecount (count @tiles)
        tilecount-delta (- current-tilecount @adapt-last-tilecount)
        efficiency (double (/ tilecount-delta adapt-backtrack-window))
        target-distance (- 1.0 efficiency)
        step-scale 0.1
        adhd-step-size (* (- 1 (rand 2)) target-distance step-scale)
        autism-step-size (* (- 1 (rand 2)) target-distance step-scale)
        ]
    (append-stats-buffer! stats-efficiency efficiency)
    ;(println "tiles:" current-tilecount
    ;         "delta:" tilecount-delta
    ;         "efficiency:" efficiency
    ;         "target-dist:" target-distance
    ;         "step-size:" adhd-step-size autism-step-size)

    ;(reset! adhd (+ @adhd adhd-step-size))
    ;(reset! autism (+ @autism autism-step-size))
    ;(println "adhd:" (format "%.2f" @adhd)
    ;         "autism:" (format "%.2f" @autism))
    (reset! adapt-last-tilecount current-tilecount)
  ))



(defn get-buffer-scale [buf]
  (if (> (count buf) 0)
    (apply max buf)
    100))

(defn init-stats-buffers []
  (reset! stats-tile-count [])
  ;(reset! stats-empty-count (atom []))
  (reset! stats-dead-count [])
  (reset! stats-backtrack [])
  (reset! stats-iter-time [])
  )


(def adapt-last-tilecount (atom 0))
(def adapt-backtrack-window 1)


