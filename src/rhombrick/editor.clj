(ns rhombrick.editor
  (:use [quil.core]
        [rhombrick.facecode]
        [rhombrick.tiling]
        [rhombrick.tiling-render]
        [rhombrick.button]
        [rhombrick.staticgeometry]
        [rhombrick.tileset-library]))

;(def current-tileset-colors {})

(def button-color {
                   :fill [0 0 0 235]
                   :stroke [0 0 0 0]
                   :fill-hot [128 128 255 192]
                   :stroke-hot [0 0 192 192]
                   :fill-active [255 0 0 192]
                   :stroke-active [255 255 255 192]
                   })

(def default-editor-state {:level 0
                           :selected [0 0 0]
                           :selected-tilecode-digit []
                           :tileset (atom ["--1--1--1--1"])
                         })

(def editor-state (atom default-editor-state))
(def library-tilesets (atom []))
(def library-tileset-index (atom 0))

(def next-digit {\- \1
                 \1 \a
                 \a \A
                 \A \-})

(defn get-selected [level]
  ((@editor-state :selected) level))


(defn set-selected [i level]
  (let [selected (get-selected level) ]
    (swap! editor-state assoc :selected
                              (assoc (@editor-state :selected) level i))))


(defn get-tileset []
  @(@editor-state :tileset))
 

(defn get-tileset-as-set []
  (set (get-tileset)))


(defn add-to-tileset [tile]
  (let [idx (count (get-tileset))]
    (swap! (@editor-state :tileset) assoc idx tile)))


(defn set-tileset [tileset]
  (println "set-tileset: " tileset)
;  (let [;tileset-set (distinct tileset)
;        num-uniq (count tileset)]
  (reset! (@editor-state :tileset) [])
  (reset! current-tileset-colors {})
  (doseq [code tileset]
    ;(when-not (set-contains-rotations? (set tileset) code)
    (add-to-tileset code)
    (let [col (compute-tile-color code)]
      (doseq [rc (rotations-preserving-symmetry code)]
        (swap! current-tileset-colors assoc rc col))))
  (if (> ((@editor-state :selected) 1) (dec (count (get-tileset))))
    (set-selected (dec (count (get-tileset))) 1))
  ;(init-dead-loci)
  ;(update-tileset-expanded tileset)
  ; (soft-init-tiler)
  )


(defn valid-level? [l]
  (and (>= l 0) (< l 3)))


(defn get-level []
  (@editor-state :level))


(defn set-level [l]
  (if (valid-level? l)
    (swap! editor-state assoc :level l)))


(defn get-max-selected-idx []
  (let [level (get-level)]
    (cond 
      (= level 1) 
        (count (get-tileset))
      (= level 2)
        12)))



(defn index-exclude [r ex] 
   "Take all indices except ex" 
    (filter #(not (ex %)) (range r))) 

(defn dissoc-idx [v & ds]
   (map v (index-exclude (count v) (into #{} ds))))


(defn replace-facecode-digit [code idx d]
  (apply str (map-indexed #(if (= %1 idx) d %2) code)))


(defn set-current-tileset-digit [tile-idx idx d]
  (when-let [tileset (get-tileset)]
    (when (< tile-idx (count tileset))
      (let [code (tileset tile-idx)
            new-code (replace-facecode-digit code idx d)
            new-tileset (assoc tileset tile-idx new-code)]
        (set-tileset new-tileset)))))


(defn key-edit-tilecode []
  (when-let[tileset (get-tileset)] ; (> (count (get-tileset)) 0)
    (let [selected-tile-idx (get-selected 1)
          selected-code (tileset selected-tile-idx)
          selected-digit-idx (get-selected 2)
          selected-digit (nth selected-code selected-digit-idx)
          new-digit (next-digit selected-digit)]
      (println "tileset:" tileset
               "selected-tile-idx:" selected-tile-idx
               "selected-code:" selected-code
               "selected-digit-idx" selected-digit-idx
               "selected-digit" selected-digit)
      (set-current-tileset-digit selected-tile-idx
                                 selected-digit-idx
                                 new-digit)
      (soft-init-tiler (get-tileset-as-set)))))



(defn level-up []
  (if (and ( < (get-level) 2)
           (> (count (get-tileset)) 0))
    (set-level (inc (@editor-state :level)))
    (key-edit-tilecode)))

(defn level-down []
  (set-level (dec (@editor-state :level))))

(defn move-left []
  (let [level (get-level)]
    (if (> level 0)
      (set-selected (mod (dec (get-selected level))
                         (get-max-selected-idx))
                    level))))
(defn move-right []
  (let [level (get-level)]
    (if (> level 0)
      (set-selected (mod (inc (get-selected level))
                         (get-max-selected-idx))
                    level))))



(defn load-library-tileset [idx]
  (when (< idx (count @library-tilesets))
    (set-tileset (@library-tilesets idx))
    (soft-init-tiler (get-tileset-as-set))))


(defn load-next-library-tileset []
  (let [new-idx (mod (inc @library-tileset-index)
                     (count @library-tilesets))]
    (reset! library-tileset-index new-idx)
    (load-library-tileset new-idx)))


(defn load-prev-library-tileset []
  (let [new-idx (mod (dec @library-tileset-index)
                     (count @library-tilesets))]
    (reset! library-tileset-index new-idx)
    (load-library-tileset new-idx)))

(defn save-current-tileset-to-library []
  (let [ts (get-tileset)]
    (save-tileset-to-library ts)
    (swap! library-tilesets conj ts)))


(defn remove-from-current-tileset [code]
  (let [new-tileset (vec (filter #(not= code %) (get-tileset)))]
    (set-tileset new-tileset)))

;(defn remove-from-current-tileset [code]
;  (swap! current-tileset-colors dissoc code)
;  (swap! current-tileset disj code))




(defn init-editor []
  (reset! editor-state default-editor-state)
  (reset! library-tilesets (load-tileset-library)))


(defn draw-selected [[x y] bscale col]
  (apply stroke col)
  (no-fill)
  (rect x y bscale bscale))


(defn draw-facecode-buttons [[x y] sc code parent-idx]
  (let [num-buttons 12
        bspace 1
        bsize (/ (- sc (* bspace (- num-buttons 1)))
                 num-buttons)
        txt-off-x (- (/ bsize 2) 4)
        txt-off-y (+ 5 (/ bsize 2))
        level (get-level)
        selected (get-selected 2)]
    (doseq [i (range num-buttons)]
      (let [bx (+ x (+ (* i bsize) (* i bspace)))
            by y
            tx (+ bx txt-off-x)
            ty (+ by txt-off-y)]
        (if (button bx by bsize bsize button-color (str (.charAt code i)))
          (println "button pressed:" code))
        (if (and (= level 2)
                 (= i selected)
                 (= parent-idx (get-selected 1)))
          (draw-selected [bx by] bsize [255 255 255 255]))
        (fill 255 255 255 255)
        (text (str (.charAt code i)) tx ty)))))


(defn draw-tile-button [[x y] code bscale parent-idx]
  (let [bx (+ x (/ bscale 2))
        by (+ y (/ bscale 2))
        col [64 64 64 190]]
    (stroke-weight 1)
    (when (button x y bscale bscale button-color code)
      (do
        (println "button pressed:" code)))
    (with-translation [bx by]
        (scale (/ bscale 6))
        (rotate-y (* (frame-count) 0.0051471))
        (no-fill)
        (stroke-weight 1)
        (draw-faces-lite rd-verts rd-faces col)
        (draw-facecode code)
        (scale 2)
        (draw-face-boundaries [0 0 0] code))))



(defn draw-rotations [[x y] code bscale]
  (let [filtered-rotations (vec (filter #(not= code %) (expand-tiles-preserving-symmetry [code])))
        num-buttons (dec (count filtered-rotations))
        bspace 1]
    (doseq [i (range (count filtered-rotations))]
      (let [i2 (mod i 12)
            bx (+ x (+ (* i2 bscale) (* i2 bspace)))
            by (+ y (* (int (/ i 12)) (+ bscale bspace)))
            rot-code (nth filtered-rotations i)]
        (draw-tile-button [bx by] rot-code bscale i)))))


(defn draw-tile-editor [[x y] code bscale parent-idx]
  (let [bx (+ x (/ bscale 2))
        by (+ y (/ bscale 2))
        col [64 64 64 190]]
    (stroke-weight 1)
    (when (button x y bscale bscale button-color code)
      (do
        (println "button pressed:" code)))
    (draw-facecode-buttons [x (+ y bscale 5)] bscale code parent-idx)
    (with-translation [bx by]
        (scale (/ bscale 6))
        (rotate-y (* (frame-count) 0.0051471))
        (no-fill)
        (stroke-weight 1)
        (draw-faces-with-info rd-verts rd-faces col)
        (no-fill)
        (draw-facecode code)
        (scale 2)
        (draw-face-boundaries [0 0 0] code))))



(defn draw-tileset-editor [[x y] tileset bscale]
  (ui-prepare)
  (let [level (get-level)
        selected (get-selected 1)
        preview-pos [x (+ y bscale 10)]
        preview-scale 320
        rotations-pos [x (+ preview-scale bscale (preview-pos 1))]
        rotations-scale 96]
    (doseq [i (range (count tileset))]
      (let [code (tileset i)
            tx (+ x (* i (+ bscale (/ bscale 8))))
            ty y]
        (draw-tile-button [tx ty] code bscale i)
        (when (and (> level 0) (= i selected))
          (draw-selected [tx ty] bscale [255 255 255 255]))
        (when (and (= level 2) (= i selected))
          (draw-tile-editor preview-pos code preview-scale i)
          (draw-selected preview-pos preview-scale [255 255 255 255])
          (draw-rotations rotations-pos code rotations-scale)))))
  (ui-finish))

