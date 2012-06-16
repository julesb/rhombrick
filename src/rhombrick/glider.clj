(ns rhombrick.glider
  (:use [rhombrick.tiling]
        [rhombrick.vector]
        [rhombrick.staticgeometry]
        [rhombrick.facecode]
    ))

(def gliders (atom []))
(def max-glider-id (atom 0))

; _______________________________________________________________________

(defn make-glider [tile face-ids]
  (let [p1 (co-verts (face-ids 0))
        p2 (vec3-scale p1 0.5)
        p4 (co-verts (face-ids 1))
        p3 (vec3-scale p4 0.5)]
    (swap! max-glider-id inc)
    (swap! gliders conj
         {:id @max-glider-id
          :current-tile tile
          :entry-face-idx (face-ids 0)
          :exit-face-idx (face-ids 1)
          :bezier-points [p1 p2 p3 p4]
          :speed 0.05
          :time 0.0})
    ))

; _______________________________________________________________________

(defn choose-glider-path [code entry-face-idx]
  (let [con-idxs (get-connected-idxs code)
        num-cons (count con-idxs)]
    (println "choose-glider-path: " code entry-face-idx)
    (if (= num-cons 1)
      [entry-face-idx entry-face-idx] ; one connection, one way out
      (if (>= (count con-idxs) 2)
        (let [face-id1 entry-face-idx
              face-id2 (rand-nth (filter #(not= entry-face-idx %)
                                 con-idxs))]
          (println "choose-glider-path ret:" [face-id1 face-id2])
          [face-id1 face-id2])
        []))))

; _______________________________________________________________________

;(defn init-gliders [num-gliders]
;  (do
;    (reset! gliders [])
;    (reset! max-glider-id 0)
;    (doseq [i (range num-gliders)]
;      (let [tile (vec (rand-nth (filter #(not= nil %) (keys @tiles))))
;            entry-idx (rand-nth (get-connected-idxs (@tiles tile)))
;            path-idxs (choose-glider-path (@tiles tile) entry-idx)]
;        (make-glider tile path-idxs)))))

(defn init-gliders [num-gliders]
  (do
    (reset! gliders [])
    (reset! max-glider-id 0)
    (if (> (count @tiles) 0)
      (doseq [i (range num-gliders)]
        (let [tile [0 0 0]
              entry-idx (first (get-connected-idxs (@tiles tile)))
              path-idxs (choose-glider-path (@tiles tile) entry-idx)]
          (if (= (count path-idxs) 2)
            (make-glider tile path-idxs)))))
    ;(println @gliders)
    ))


; _______________________________________________________________________

(defn get-gliders-on-tile [pos]
  (filter #(= (% :current-tile) pos) @gliders))

; _______________________________________________________________________

(defn update-glider-value [id k v]
  (let [glider (first (filter #(= (% :id) id) @gliders))]
    (if (not (seq glider))
      nil
      (reset! gliders 
             (conj (filter #(not= (% :id) id) @gliders)
                    (assoc glider k v))))))
 

(defn update-glider-bezier [id face-ids]
  (let [p1 (co-verts (face-ids 0))
        p2 (vec3-scale p1 0.5)
        p4 (co-verts (face-ids 1))
        p3 (vec3-scale p4 0.5)]
    (update-glider-value id :bezier-points [p1 p2 p3 p4])))
; _______________________________________________________________________

(defn is-traversable? [tilecode]
  (not (or 
    (= 0 (count (get-connected-idxs tilecode)))
    (= tilecode "xxxxxxxxxxxx"))))

(defn update-gliders []
  (doseq [glider @gliders]
    (let [next-tile-pos (get-neighbour (glider :current-tile)
                                       (glider :exit-face-idx))
          new-glider-time (+ (glider :time) (glider :speed))]
      (if (and (>= new-glider-time 1.0)
               (not= nil next-tile-pos)
               ;(> 0 (count (get-connected-idxs (@tiles next-tile-pos))))
               )
        ; have crossed tile boundary..
        (let [next-tile-code (@tiles next-tile-pos)
              next-entry-face-idx (connecting-faces (glider :exit-face-idx))
              next-glider-path (choose-glider-path next-tile-code
                                                   next-entry-face-idx )]
          (if (not (is-traversable? next-tile-code))
            ; the next tile is not traversable or doesnt exist
            ; so reverse direction
            (let [old-entry-idx (glider :entry-face-idx)
                  old-exit-idx (glider :exit-face-idx)]
              (update-glider-value (glider :id) :entry-face-idx
                                                old-exit-idx)
              (update-glider-value (glider :id) :exit-face-idx
                                                old-entry-idx)
              (update-glider-value (glider :id) :time 0.0)
              (update-glider-bezier (glider :id) 
                                    [old-exit-idx old-entry-idx]))
            
            (do
              (update-glider-value (glider :id) :time (- new-glider-time 1))
              (update-glider-value (glider :id) :current-tile next-tile-pos)
              (update-glider-value (glider :id) 
                               :entry-face-idx (next-glider-path 0))
              (update-glider-value (glider :id) 
                               :exit-face-idx (next-glider-path 1))
              (update-glider-bezier (glider :id) next-glider-path))))
        
        (update-glider-value (glider :id) :time new-glider-time)
          ))))

        ; have crossed tile boundary (time >= 1).. 
        ; - determine next tile
        ; - if next tile exists and has > 0 connections
        ;   - choose next path
        ;   - subtract 1 from new-glider-time
        ;   - update glider values
      ; else
        ; increment glider time
        ; update glider   




; _______________________________________________________________________

