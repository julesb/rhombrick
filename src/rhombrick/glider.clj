(ns rhombrick.glider
  (:use [quil.core]
        [rhombrick.tiling]
        [rhombrick.tiling-render]
        [rhombrick.vector]
        [rhombrick.staticgeometry]
        [rhombrick.obj-loader]
        ;[rhombrick.facecode]
        [overtone.osc]
    ))

(def gliders (atom []))
(def num-gliders 2)
(def max-glider-id (atom 0))

(def OSCPORT 4242)
(def client (osc-client "localhost" OSCPORT))



;(def glider-model (load-obj "data/rhombic_dodecahedron.obj"))
(def glider-model (get-obj-face-verts (load-obj "data/smooth_spaceship.obj")))



; _______________________________________________________________________

(defn make-glider [tile face-ids]
  (let [;p1 (co-verts (face-ids 0))
        ;p2 (vec3-scale p1 0.5)
        ;p4 (co-verts (face-ids 1))
        ;p3 (vec3-scale p4 0.5)
        new-maxid (inc @max-glider-id)
        t (- 1.0 (/ new-maxid num-gliders))
        col (rand-nth rd-face-colors)]
    (reset! max-glider-id new-maxid)
    (swap! gliders conj
         {:id @max-glider-id
          :current-tile tile
          :entry-face-idx (face-ids 0)
          :exit-face-idx (face-ids 1)
          :speed 0.02
          :time t
          :color col
          }
    )))

; _______________________________________________________________________

(defn choose-glider-path-old [code entry-face-idx]
  (let [con-idxs (get-connected-idxs code)
        num-cons (count con-idxs)]
    (if (= num-cons 1)
      [entry-face-idx entry-face-idx] ; one connection, one way out
      (if (>= (count con-idxs) 2)
        (let [face-id1 entry-face-idx
              face-id2 (rand-nth (filter #(not= entry-face-idx %)
                                 con-idxs))]
          [face-id1 face-id2])
        []))))


(defn choose-left-or-right [connected-idxs entry-idx]
  (let [i (.indexOf connected-idxs entry-idx)
        n (count connected-idxs)
        r (rand-int 2)]
    (if (= r 0)
      (nth connected-idxs (mod (dec i) n))
      (nth connected-idxs (mod (inc i) n)))))
        

(defn choose-glider-path [code entry-face-idx]
  (let [con-idxs (get-connected-idxs code)
        num-cons (count con-idxs)]
    (if (= num-cons 1)
      [entry-face-idx entry-face-idx] ; one connection, one way out
      (if (>= (count con-idxs) 2)
        [entry-face-idx (choose-left-or-right con-idxs entry-face-idx)]
        []))))

; _____________________________________________________________


(defn get-glider-pos [id]
  (let [glider (first (filter #(= (% :id) id) @gliders))]
    (if (= glider nil)
      [0 0 0]
      (let [tile (glider :current-tile)
            entry-idx (glider :entry-face-idx)
            exit-idx (glider :exit-face-idx)
            t (glider :time)
            p1 ((@current-topology :face-centers) entry-idx)
            p2 (vec3-scale p1 0.5)
            p4 ((@current-topology :face-centers) exit-idx)
            p3 (vec3-scale p4 0.5)
            bx (vec (map #(% 0) [p1 p2 p3 p4]))
            by (vec (map #(% 1) [p1 p2 p3 p4]))
            bz (vec (map #(% 2) [p1 p2 p3 p4]))
            gx (bezier-point (bx 0) (bx 1) (bx 2) (bx 3) t)
            gy (bezier-point (by 0) (by 1) (by 2) (by 3) t)
            gz (bezier-point (bz 0) (bz 1) (bz 2) (bz 3) t)
            pos (vec3-add [gx gy gz] tile)]
      pos))))


(defn get-glider-nextpos [id]
  (let [glider (first (filter #(= (% :id) id) @gliders))]
    (if (= glider nil)
      [0 0 0]
      (let [tile (glider :current-tile)
            entry-idx (glider :entry-face-idx)
            exit-idx (glider :exit-face-idx)
            t (+ (glider :time) (* 2.0 (glider :speed)))
            p1 ((@current-topology :face-centers) entry-idx)
            p2 (vec3-scale p1 0.5)
            p4 ((@current-topology :face-centers) exit-idx)
            p3 (vec3-scale p4 0.5)
            bx (vec (map #(% 0) [p1 p2 p3 p4]))
            by (vec (map #(% 1) [p1 p2 p3 p4]))
            bz (vec (map #(% 2) [p1 p2 p3 p4]))
            gx (bezier-point (bx 0) (bx 1) (bx 2) (bx 3) t)
            gy (bezier-point (by 0) (by 1) (by 2) (by 3) t)
            gz (bezier-point (bz 0) (bz 1) (bz 2) (bz 3) t)
            pos (vec3-add [gx gy gz] tile)]
      pos))))

; _______________________________________________________________________



;(defn update-glider-values [id k v]
;  (update-in @gliders


(defn update-glider-value [id k v]
  (let [glider (first (filter #(= (% :id) id) @gliders))]
    (if (not (seq glider))
      nil
      (reset! gliders 
              (conj (filter #(not= (% :id) id) @gliders)
                    (assoc glider k v))))))


;(defn update-glider-value2 [id newvals]
;  (let [glider (first (filter #(= (% :id) id) @gliders))]
;    (if (not (seq glider))
;      nil
;      (swap! @gliders assoc 
;      )))
; _______________________________________________________________________


(defn init-gliders [num-gliders]
  (do
    (reset! gliders [])
    (reset! max-glider-id 0)
    ;(when (> (count @tiles) 0)
    (when (> (count (@tiler-state :tiles)) 0)
      (doseq [i (range (+ 2 num-gliders))]
        (let [tile (rand-nth (keys (@tiler-state :tiles) ))
              entry-idx (first (get-connected-idxs ((@tiler-state :tiles) tile)))
              path-idxs (choose-glider-path ((@tiler-state :tiles) tile) entry-idx)]
          (if (= (count path-idxs) 2)
            (make-glider tile path-idxs)))))
    ;(update-glider-value (@gliders 0) :time 0.3)
    ;(update-glider-value (@gliders 1) :time 0.6)
    ;(update-glider-value 1 :speed 0.01)
    ;(println @gliders)
    ))

; _______________________________________________________________________


(defn get-gliders-on-tile [pos]
  (filter #(= (% :current-tile) pos) @gliders))


(defn is-traversable? [tilecode]
  (not 
    (or 
      (= 0 (count (get-connected-idxs tilecode)))
      (= tilecode "------------")
      (= tilecode nil)
      (not= (count tilecode) (@current-topology :num-faces))
      )))

; _______________________________________________________________________


(defn get-glider [id]
  (first (filter #(= (% :id) id) @gliders)))


(defn update-gliders-test []
  (doseq [glider @gliders]
    (let [next-tile-pos [0 0 0]
          new-glider-time (mod (+ (glider :time) (glider :speed)) 1.0) ]
      (update-glider-value (glider :id) :time new-glider-time))))
        

(defn on-camera-tile? [gid]
  (= ((get-glider gid) :currrent-tile)
    ((get-glider 1) :current-tile)))


(defn update-gliders []
  (doseq [glider @gliders]
    (let [exit-idx (glider :exit-face-idx)
          exit-idx-cl (if (< exit-idx (@current-topology :num-faces))
                        exit-idx
                        (- (@current-topology :num-faces) 1))

          next-tile-pos (get-neighbour-pos (glider :current-tile)
                                       exit-idx-cl) ;(glider :exit-face-idx))
          new-glider-time (+ (glider :time) (glider :speed))]
      (if (>= new-glider-time 1.0)
        ; have crossed tile boundary..
        (let [next-tile-code ((@tiler-state :tiles) next-tile-pos)
              next-entry-face-idx ((@current-topology :op-face-idx) exit-idx-cl)
              next-glider-path (choose-glider-path next-tile-code
                                                   next-entry-face-idx )]
          (if (is-traversable? next-tile-code)
            (do
              (if (and 
                    (= (glider :id) 1)
                    (> (count (get-connected-idxs next-tile-code)) 2))
                (osc-send client "/rhombrick.game" "boundary" (float (glider :id))))
              (update-glider-value (glider :id)
                                   :time (- new-glider-time
                                            (int new-glider-time)))
              (update-glider-value (glider :id)
                                   :current-tile next-tile-pos)
              (update-glider-value (glider :id) 
                                   :entry-face-idx (next-glider-path 0))
              (update-glider-value (glider :id) 
                                   :exit-face-idx (next-glider-path 1)))
            ; the next tile is not traversable or doesnt exist
            ; so reverse direction
            (do
              (if (= (glider :id) 1)
                (osc-send client "/rhombrick.game" "hitblock" (float (glider :id))))
              (let [old-entry-idx (glider :entry-face-idx)
                    old-exit-idx (glider :exit-face-idx)]
                (update-glider-value (glider :id) :entry-face-idx
                                                  old-exit-idx)
                (update-glider-value (glider :id) :exit-face-idx
                                                  old-entry-idx)
                (update-glider-value (glider :id) :time 0.0)))))
        ; still in tile so just increment time/pos
        (update-glider-value (glider :id) :time new-glider-time)))))

        ; have crossed tile boundary (time >= 1).. 
        ; - determine next tile
        ; - if next tile exists and has > 0 connections
        ;   - choose next path
        ;   - subtract 1 from new-glider-time
        ;   - update glider values
      ; else
        ; increment glider time
        ; update glider   



(defn draw-gliders [frame]
  (push-style)
  (no-stroke)
  ;(stroke-weight 1)
  ;(stroke 255 255 192 192)
  (doseq [glider @gliders]
    (let [pos (get-glider-pos (glider :id))
          pos2 (get-glider-nextpos (glider :id))
          [dx dy dz] (vec3-normalize (vec3-sub pos pos2))
          az (Math/atan2 dy dx)
          el (- (Math/asin dz))
          col (glider :color)
          tile (glider :current-tile)
          ]
      (if (contains? (@tiler-state :tiles) tile)
        (with-translation pos
          (fill (col 0) (col 1) (col 2) 255)
            (scale 0.005)
            (rotate az 0 0 1)
            (rotate el 0 1 0)
            ;(box 4 1 1)
            ;(box 1 3 1)
            (draw-obj glider-model [128 128 255 255])
            ;(draw-obj (glider-model :vertex) (glider-model :face) [128 128 255 255])
                          ))))
  (pop-style)
  )

; _______________________________________________________________________

