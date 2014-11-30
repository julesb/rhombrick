(ns rhombrick.jigsaw
  (:refer-clojure :exclude [import use])
  (:use [scad-clj.scad]
        [scad-clj.model]
        [rhombrick.tiling]
        [rhombrick.tilecode]
        [rhombrick.staticgeometry :as geom]
        [rhombrick.tile-shape-2d]
        [rhombrick.tiling-render :only [get-tile-color]]
        [rhombrick.vector]
        [ordered.map]
        ))


(reset! current-topology (topologies :hexagon))


; "d-4-D-" "d--D--"

(def piece
  (->> (make-jigsaw-piece "-4-Dd4" @current-topology 32)
       (apply concat)
       (map #(vec [(% 0) (% 1)]))
       (vec)))


(defn gcode-moveto
  ([x y]
    (format "G1 X%.4f Y%.4f" x y))
  ([[x y z]]
    (format "G1 X%.4f Y%.4f Z%.4f" x y z)))


; tool change routine:
;G1 Z200 F7800
;M117 (Change pen [1 2 3])
;G1 Z200 F7800
;G4 P5000; pause

(def gcode-wait-tool-change
  (apply str
    (concat ;["G1 Z200 F7800\n"] ; move up 200mm
            ;["M117 change pen\n"]
            ;["M226\n"] ; gcode initiated pause
            ["G1 Z200 F7800\n"]
            ["G4 P120000\n"]))
)


(defn tile-gcode [pos code topo scale speed]
  (let [zlift 5.0
        travel-speed (* speed 4)
        verts (->> (apply concat (make-jigsaw-piece code topo 8))
                   (map #(vec3-scale % scale))
                   (map #(vec3-add (vec3-scale pos scale) %)))
        draw-verts (rest (map #(gcode-moveto (% 0) (% 1)) verts))]
    (doseq [v verts]
      (if (> (vec3-length v) 120.0)
        (println v "is out of radius")))

    (apply str (interpose "\n"
      (concat [(str "G1 F" travel-speed)]
              [(gcode-moveto [((first verts) 0) ((first verts) 1) zlift])] ; above first point
              [(gcode-moveto [((first verts) 0) ((first verts) 1) 0.0])] ; touch paper at first point
              [(str "G1 F" speed)]
              draw-verts
              [(gcode-moveto ((first verts) 0) ((first verts) 1))] ; back to the first point
              [(str "G1 F" travel-speed)]
              [(gcode-moveto [((first verts) 0) ((first verts) 1) zlift])] ; above first point
              )))))


(defn sort-xy [tiles]
  (->> tiles
       (into [])
       (clojure.core/sort-by #((key %) 0))
       (clojure.core/sort-by #((key %) 1))
       (ordered-map)))


(defn do-tiling-gcode [tiles topo scale speed]
  (let [tile-fn (fn [pos code] (apply str (tile-gcode pos code topo scale speed) "\n"))
        tiles-sorted (sort-xy tiles)]
    (concat
      ;["G28\n"]
      (map #(tile-fn (key %) (val %)) tiles-sorted))))


; write muticolor tiling to single gcode file with pauses for tool/pen change
(defn write-tiling-gcode-with-toolchange [ts topo fname]
  (spit fname (apply str
               (->> (group-by #(get-tile-color (val %)) (ts :tiles))
                    (map #(apply str (do-tiling-gcode (val %) topo 3.5 1500)))
                    (interpose gcode-wait-tool-change)))))


; write muticolor tiling to separate gcode file per color
(defn write-split-tiling-gcode [ts topo]
  (doseq [tiles (group-by #(get-tile-color (val %)) (ts :tiles))]
    (let [fname (apply str "tiling-gcode" (key tiles) ".gcode")]
      (println "writing" (count (vals tiles)) "tiles to file" fname)
      (spit fname (apply str (do-tiling-gcode (val tiles) topo 3.5 1500))))))



(defn get-piece-verts-2d [code topo res ]
  (->> (make-jigsaw-piece code topo res)
       (apply concat)
       (map #(vec [(% 0) (% 1)])) ; make 2d
       (vec)))


(defn tile-spheretop [code topo]
  (intersection
    (difference
      (scale [1 1 1]
        (extrude-linear {:height 1.0 :twist 0.0}
           ;(sphere 10)
           (polygon (get-piece-verts-2d code topo 8))
           )
         )
      ;(translate [0 0 5]
      ;  (extrude-rotate
      ;    (translate [3 0 0] (circle 0.25))))
      ;(translate [0 0 2.5] (scale [1 1 0.2] (with-fn 64 (sphere 8.0))))
      ;                )
      ;(rotate (/ Math/PI 6.0) [0 0 1] (with-fn 6 (cylinder 4.75 10)))
      )
    (union
      (translate [0 0 -0.25] (with-fn 64 (cylinder 1.75 0.5)))
      (translate [0 0 0.25]
        (scale [1 1 0.2]
          (with-fn 64 (sphere 2.0))))
    )))


(defn tile-simple [code topo]
  (extrude-linear {:height 1.0 :twist 0.0}
    (polygon (get-piece-verts-2d code topo 8))))


(defn do-tiling  [tiles topo]
  (let [tile-fn (fn [pos code]
                  (color (vec3-scale (get-tile-color code) (/ 1.0 255.0))
                     (translate pos
                        (tile-simple code topo))))]
    (union (map #(tile-fn (key %) (val %)) tiles))))


(defn make-sheet [width-mm height-mm nrows ncols tile-radius-mm margin]
  ;(let [tiles (@tiler-state :tiles)
  (let [tiles (into {} (map #(vec [%1 (make-normal-random-tilecode)])
                            (apply concat
                              (for [j (range nrows)]
                                (for [i (range ncols)]
                                  (let [offset (* (mod i 2) 0.5)
                                        x (+ margin
                                             (* i tile-radius-mm))
                                        y (+ margin
                                             (* offset tile-radius-mm)
                                             (* j tile-radius-mm))]
                                    [x y]))))))]
    (do-tiling {:tiles tiles} @current-topology)))

;(def sheet (make-sheet 295 210 10 20 5 5))


(def tile
  (union
    (scale [10 10 1]
      (extrude-linear {:height 5.0}
         ;(sphere 10)
         (polygon piece)
         )
       )
    ;                )
      (translate [0 0 5]
      (extrude-rotate
        (translate [5 0 0] (circle 2.5))))

      (translate [0 0 6.0]
        (rotate (/ Math/PI 6.0) [0 0 1]
          (with-fn 6 (cylinder [4.75 2] 2))))
  ))

;(def tiles (scale [10 10 5] (do-tiling (@tiler-state :tiles) @current-topology)))


(defn write-split-tiling [ts topo]
  (doseq [tiles (group-by #(get-tile-color (val %)) (ts :tiles))]
    (let [fname (apply str "tiling-" (key tiles) ".scad")]
      (println "writing" (count (vals tiles)) "tiles to file" fname)
      (spit fname (write-scad
                    (scale [10 10 5]
                      (do-tiling (val tiles) topo)))))))


;(write-tiling-gcode-with-toolchange @tiler-state @current-topology "tiling-with-tc.gcode")
;(write-split-tiling-gcode @tiler-state @current-topology)

(def tiling-gcode (apply str (do-tiling-gcode (@tiler-state :tiles) @current-topology 3.5 1500)))
(spit "tiling-gcode.gcode" tiling-gcode)

;(write-split-tiling @tiler-state @current-topology)

;(spit "tiles.scad" (write-scad tiles))
;(spit "sheet.scad" (write-scad sheet))
;(spit "piece-spheretop.scad" (write-scad (tile-spheretop "4-D-d-" @current-topology)))
;(spit "piece.scad" (write-scad tile))


;(set-tileset ["--CC-C" "2-CC-C" "-2CC-C" "c-c-c-" "2-2---" "22----" "2-2-2-"])
